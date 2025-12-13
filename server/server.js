// Connect4 Game Server - Integrates with SWI-Prolog backend
const express = require('express');
const { spawn } = require('child_process');
const cors = require('cors');
const path = require('path');

const app = express();
app.use(cors());
app.use(express.static(path.join(__dirname, 'public')));
app.use(express.json());

app.get('/', (req, res) => {
    res.sendFile(path.join(__dirname, 'public', 'index.html'));
});

// In-memory game storage
const games = new Map();

class Connect4Game {
    constructor(player1, player2) {
        this.id = Date.now().toString();
        this.player1 = player1; // 'basic', 'minmax', 'random', 'human'
        this.player2 = player2;
        this.boardStates = []; // Store all intermediate board states
        this.status = 'waiting'; // 'waiting', 'playing', 'finished'
        this.currentPlayer = 'X'; // X goes first
        this.board = Array(42).fill(null);
        this.prologProcess = null;
        this.moveHistory = [];
        this.outputBuffer = '';
        this.waitingForHumanInput = false;
        this.gameResult = null;
        this.moveNumber = 0;
        this.isAIvsAI = (player1 !== 'human' && player2 !== 'human');

        // Board collection state (persists across output chunks)
        this.collectingBoard = false;
        this.boardLines = [];
        this.pendingPlayer = null;

        games.set(this.id, this);
    }

    start() {
        this.status = 'playing';

        // Set environment variables for AI types
        const env = {
            ...process.env,
            p1: this.player1 === 'human' ? 'basic' : this.player1,
            p2: this.player2 === 'human' ? 'basic' : this.player2
        };

        // Start Prolog game
        this.prologProcess = spawn('swipl', [
            '-s', path.join(__dirname, '../src/game.pl'),
            '-s', path.join(__dirname, '../src/gameover.pl'),
            '-s', path.join(__dirname, '../src/bootstrap.pl'),
            '-s', path.join(__dirname, '../src/ia/basic.pl'),
            '-s', path.join(__dirname, '../src/ia/minmax.pl'),
            '-s', path.join(__dirname, '../src/ia/random.pl'),
            '-g', 'init, halt.'
        ], {
            env,
            cwd: path.join(__dirname, '..')
        });

        // Parse Prolog output
        this.prologProcess.stdout.on('data', (data) => {
            const output = data.toString();
            this.outputBuffer += output;
            this.parsePrologOutput(output);
        });

        this.prologProcess.stderr.on('data', (data) => {
            console.error(`[Game ${this.id}] Prolog stderr:`, data.toString());
        });

        this.prologProcess.on('close', (code) => {
            console.log(`[Game ${this.id}] Prolog process ended with code ${code}`);
            if (this.status !== 'finished') {
                this.status = 'finished';
            }
        });
    }

    parsePrologOutput(output) {
        console.log(`[Game ${this.id}] Prolog output:\n${output}`);

        const lines = output.split('\n');

        for (const line of lines) {
            const trimmed = line.trim();

            // Detect "New turn for: X" or "New turn for: O"
            const turnMatch = trimmed.match(/^New turn for:\s*([XO])/);
            if (turnMatch) {
                // Save the player for this turn
                this.pendingPlayer = turnMatch[1] === 'X' ? 'X' : 'O';
                this.currentPlayer = this.pendingPlayer;
                this.collectingBoard = true;
                this.boardLines = [];
                this.moveNumber++;
                continue;
            }

            // Collect board lines (7 cells per line, 6 lines total)
            if (this.collectingBoard && trimmed.length > 0 && !trimmed.includes('Game over')) {
                // Each cell is represented as `. ` (empty), `X ` (player X), or `O ` (player O)
                const cells = trimmed.split(' ').filter(c => c.length > 0);

                if (cells.length === 7) {
                    this.boardLines.push(cells);

                    // When we have all 6 rows, parse the complete board
                    if (this.boardLines.length === 6) {
                        const board = this.parseBoardFromLines(this.boardLines);

                        // Store this board state
                        this.boardStates.push({
                            move: this.moveNumber,
                            player: this.pendingPlayer,
                            board: [...board],
                            timestamp: Date.now()
                        });

                        // Update current board
                        this.board = [...board];

                        // Reset collection state
                        this.collectingBoard = false;
                        this.boardLines = [];
                    }
                }
            }

            // Check for game over
            const gameOverMatch = trimmed.match(/^Game over\s*!\s*\((.+)\)/);
            if (gameOverMatch) {
                this.status = 'finished';
                this.gameResult = gameOverMatch[1];
                this.collectingBoard = false; // Stop collecting
                console.log(`[Game ${this.id}] Game finished: ${this.gameResult}`);

                // For AI vs AI games, we're done collecting data
                if (this.isAIvsAI) {
                    console.log(`[Game ${this.id}] AI vs AI game completed with ${this.boardStates.length} board states`);
                }
            }

            // Detect human input prompt (|:)
            if (trimmed.includes('|:')) {
                this.waitingForHumanInput = true;
                console.log(`[Game ${this.id}] Waiting for human input`);
            }
        }
    }

    parseBoardFromLines(boardLines) {
        // Convert 6 rows x 7 columns into a flat array of 42 cells
        // Prolog board layout: 0-6 (top row) ... 35-41 (bottom row)
        const board = Array(42).fill(null);

        for (let row = 0; row < 6; row++) {
            for (let col = 0; col < 7; col++) {
                const cell = boardLines[row][col];
                const index = row * 7 + col;

                if (cell === 'X') {
                    board[index] = 'X';
                } else if (cell === 'O') {
                    board[index] = 'O';
                } else {
                    board[index] = null;
                }
            }
        }

        return board;
    }

    makeMove(column, player) {
        if (this.status !== 'playing') {
            throw new Error('Game is not active');
        }

        if (this.currentPlayer !== player) {
            throw new Error('Not your turn');
        }

        if (column < 1 || column > 7) {
            throw new Error('Column must be between 1 and 7');
        }

        // Send move to Prolog process
        if (this.prologProcess && this.prologProcess.stdin) {
            this.prologProcess.stdin.write(`${column}.\n`);
            this.waitingForHumanInput = false;
            console.log(`[Game ${this.id}] Sent move: column ${column}`);
        } else {
            throw new Error('Prolog process not available');
        }
    }

    getState() {
        return {
            id: this.id,
            player1: this.player1,
            player2: this.player2,
            status: this.status,
            currentPlayer: this.currentPlayer,
            board: this.board,
            boardStates: this.boardStates,
            moveHistory: this.moveHistory,
            waitingForHumanInput: this.waitingForHumanInput,
            gameResult: this.gameResult,
            totalMoves: this.boardStates.length,
            isAIvsAI: this.isAIvsAI
        };
    }

    destroy() {
        if (this.prologProcess) {
            this.prologProcess.kill();
        }
        games.delete(this.id);
    }
}

// API Routes

// Start a new game
app.post('/api/games', (req, res) => {
    const { player1 = 'basic', player2 = 'basic' } = req.body;

    if (!['basic', 'minmax', 'random', 'human'].includes(player1) ||
        !['basic', 'minmax', 'random', 'human'].includes(player2)) {
        return res.status(400).json({
            error: 'Invalid player type. Must be: basic, minmax, random, or human'
        });
    }

    const game = new Connect4Game(player1, player2);
    game.start();

    res.json({
        message: 'Game started',
        game: game.getState()
    });
});

// Get game state
app.get('/api/games/:id', (req, res) => {
    const game = games.get(req.params.id);
    if (!game) {
        return res.status(404).json({ error: 'Game not found' });
    }
    res.json(game.getState());
});

// Make a move
app.post('/api/games/:id/move', (req, res) => {
    const game = games.get(req.params.id);
    if (!game) {
        return res.status(404).json({ error: 'Game not found' });
    }

    const { column, player } = req.body;

    try {
        game.makeMove(column, player);
        res.json({
            message: 'Move made successfully',
            game: game.getState()
        });
    } catch (error) {
        res.status(400).json({ error: error.message });
    }
});

// Delete/end game
app.delete('/api/games/:id', (req, res) => {
    const game = games.get(req.params.id);
    if (!game) {
        return res.status(404).json({ error: 'Game not found' });
    }

    game.destroy();
    res.json({ message: 'Game ended' });
});

// List all games
app.get('/api/games', (req, res) => {
    const gameList = Array.from(games.values()).map(game => game.getState());
    res.json(gameList);
});

// Health check
app.get('/health', (req, res) => {
    res.json({
        status: 'ok',
        games: games.size,
        timestamp: new Date().toISOString()
    });
});

const PORT = process.env.PORT || 3000;
app.listen(PORT, () => {
    console.log(`🚀 Connect4 server running on port ${PORT}`);
    console.log('🌐 Web interface: http://localhost:' + PORT);
    console.log('📡 API endpoints:');
    console.log('  POST   /api/games          - Start new game');
    console.log('  GET    /api/games/:id      - Get game state');
    console.log('  POST   /api/games/:id/move - Make a move');
    console.log('  DELETE /api/games/:id      - End game');
    console.log('  GET    /api/games          - List all games');
    console.log('  GET    /health             - Health check');
    console.log('');
    console.log('Example AI vs AI:');
    console.log(`  curl -X POST http://localhost:${PORT}/api/games \\`);
    console.log('    -H "Content-Type: application/json" \\');
    console.log('    -d \'{"player1":"basic","player2":"random"}\'');
    console.log('');
    console.log('Example Human vs AI:');
    console.log(`  curl -X POST http://localhost:${PORT}/api/games \\`);
    console.log('    -H "Content-Type: application/json" \\');
    console.log('    -d \'{"player1":"human","player2":"minmax"}\'');
});
