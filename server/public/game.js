// Enhanced Connect4 Frontend with Board History and Replay
class Connect4GameUI {
    constructor() {
        this.currentGame = null;
        this.apiBase = 'http://localhost:3000';
        this.replayInterval = null;
        this.currentReplayIndex = 0;
        this.pollInterval = null;
        this.init();
    }

    init() {
        this.bindEvents();
        this.createBoard();
    }

    bindEvents() {
        document.getElementById('startGame').addEventListener('click', () => this.startGame());
        document.getElementById('resetGame').addEventListener('click', () => this.resetGame());

        // Replay controls
        document.getElementById('replayPlay').addEventListener('click', () => this.startReplay());
        document.getElementById('replayPause').addEventListener('click', () => this.pauseReplay());
        document.getElementById('replayReset').addEventListener('click', () => this.resetReplay());
        document.getElementById('prevState').addEventListener('click', () => this.previousState());
        document.getElementById('nextState').addEventListener('click', () => this.nextState());
    }

    createBoard() {
        const board = document.getElementById('gameBoard');
        board.innerHTML = '';

        // Create board cells (6 rows x 7 columns = 42 cells)
        for (let i = 0; i < 42; i++) {
            const cell = document.createElement('div');
            cell.className = 'cell';
            cell.id = `cell-${i}`;
            const column = i % 7;
            cell.dataset.column = column;

            const piece = document.createElement('div');
            piece.className = 'piece empty';
            piece.id = `piece-${i}`;
            cell.appendChild(piece);

            // Add click handler for human moves
            cell.addEventListener('click', () => {
                if (this.currentGame && this.canMakeMove()) {
                    this.makeMove(column + 1); // Convert to 1-based column
                }
            });

            board.appendChild(cell);
        }
    }

    async startGame() {
        const player1 = document.getElementById('player1').value;
        const player2 = document.getElementById('player2').value;

        try {
            const response = await fetch(`${this.apiBase}/api/games`, {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json'
                },
                body: JSON.stringify({ player1, player2 })
            });

            const data = await response.json();

            if (response.ok) {
                this.currentGame = data.game;
                this.currentReplayIndex = 0;
                this.hideError();
                this.showGameElements();
                this.updateGameState();

                // Start polling for game updates
                this.startPolling();
            } else {
                this.showError(data.error || 'Failed to start game');
            }
        } catch (error) {
            this.showError('Failed to connect to server: ' + error.message);
        }
    }

    startPolling() {
        // Clear any existing poll interval
        if (this.pollInterval) {
            clearInterval(this.pollInterval);
        }

        // Poll more frequently for updates
        const pollFrequency = this.currentGame.isAIvsAI ? 500 : 1000;

        this.pollInterval = setInterval(async () => {
            if (!this.currentGame || this.currentGame.status === 'finished') {
                clearInterval(this.pollInterval);
                this.pollInterval = null;
                return;
            }

            await this.updateGameState();
        }, pollFrequency);
    }

    async makeMove(column) {
        if (!this.currentGame) return;

        const currentPlayer = this.currentGame.currentPlayer;

        try {
            const response = await fetch(`${this.apiBase}/api/games/${this.currentGame.id}/move`, {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json'
                },
                body: JSON.stringify({
                    column: column,
                    player: currentPlayer
                })
            });

            const data = await response.json();

            if (response.ok) {
                this.currentGame = data.game;
                this.hideError();
                // Update will happen via polling
            } else {
                this.showError(data.error || 'Invalid move');
            }
        } catch (error) {
            this.showError('Failed to make move: ' + error.message);
        }
    }

    async updateGameState() {
        if (!this.currentGame) return;

        try {
            const response = await fetch(`${this.apiBase}/api/games/${this.currentGame.id}`);
            const gameState = await response.json();

            const previousStateCount = this.currentGame.boardStates ? this.currentGame.boardStates.length : 0;
            const newStateCount = gameState.boardStates ? gameState.boardStates.length : 0;

            this.currentGame = gameState;

            // Update UI
            this.renderBoard();
            this.updateStatus();
            this.updateMoveHistory();
            this.updateReplayControls();

            // If new board states arrived, advance replay index for AI vs AI
            if (this.currentGame.isAIvsAI && newStateCount > previousStateCount) {
                this.currentReplayIndex = newStateCount - 1;
            }

            // If game finished and it's AI vs AI, show replay controls
            if (gameState.status === 'finished' && this.currentGame.isAIvsAI) {
                console.log(`AI vs AI game finished with ${newStateCount} moves`);
                this.showReplayControls();
            }
        } catch (error) {
            console.error('Failed to update game state:', error);
        }
    }

    renderBoard(boardState = null) {
        if (!this.currentGame && !boardState) return;

        const board = boardState || this.currentGame.board;

        for (let i = 0; i < 42; i++) {
            const piece = document.getElementById(`piece-${i}`);
            const cell = document.getElementById(`cell-${i}`);

            // Reset classes
            piece.className = 'piece';
            cell.className = 'cell';

            if (board[i] === 'X') {
                piece.classList.add('player1');
                piece.textContent = '🔴';
                cell.classList.add('occupied');
            } else if (board[i] === 'O') {
                piece.classList.add('player2');
                piece.textContent = '🟡';
                cell.classList.add('occupied');
            } else {
                piece.classList.add('empty');
                piece.textContent = '';
            }

            // Disable cells if game is finished or not human's turn
            if (!this.canMakeMove()) {
                cell.classList.add('disabled');
            }
        }
    }

    updateStatus() {
        const statusDiv = document.getElementById('gameStatus');
        const statusText = this.getStatusText();

        statusDiv.textContent = statusText;
        statusDiv.className = `game-status status-${this.currentGame.status}`;
    }

    getStatusText() {
        if (!this.currentGame) return '';

        switch (this.currentGame.status) {
            case 'waiting':
                return 'Setting up game...';
            case 'playing':
                if (this.currentGame.isAIvsAI) {
                    const playerSymbol = this.currentGame.currentPlayer === 'X' ? '🔴' : '🟡';
                    return `AI vs AI in progress... ${playerSymbol}'s turn (Move ${this.currentGame.totalMoves})`;
                } else if (this.currentGame.waitingForHumanInput) {
                    return `Your turn! Click a column to drop your piece.`;
                } else {
                    const playerSymbol = this.currentGame.currentPlayer === 'X' ? '🔴' : '🟡';
                    return `${playerSymbol} is thinking...`;
                }
            case 'finished':
                if (this.currentGame.gameResult) {
                    return `Game Over! ${this.currentGame.gameResult}`;
                } else {
                    return 'Game Over!';
                }
            default:
                return 'Unknown status';
        }
    }

    updateMoveHistory() {
        const movesDiv = document.getElementById('moves');
        const historyDiv = document.getElementById('moveHistory');

        if (!this.currentGame.boardStates || this.currentGame.boardStates.length === 0) {
            historyDiv.classList.add('hidden');
            return;
        }

        historyDiv.classList.remove('hidden');
        movesDiv.innerHTML = '';

        this.currentGame.boardStates.forEach((state, index) => {
            const moveItem = document.createElement('div');
            moveItem.className = 'move-item';
            const playerSymbol = state.player === 'X' ? '🔴' : '🟡';
            moveItem.textContent = `Move ${state.move}: ${playerSymbol}`;
            moveItem.dataset.index = index;
            moveItem.addEventListener('click', () => this.jumpToState(index));

            // Highlight active move in replay
            if (index === this.currentReplayIndex) {
                moveItem.classList.add('active');
            }

            movesDiv.appendChild(moveItem);
        });
    }

    updateReplayControls() {
        const statesDiv = document.getElementById('boardStates');
        const stateInfo = document.getElementById('stateInfo');
        const replayStatus = document.getElementById('replayStatus');

        if (!this.currentGame.boardStates || this.currentGame.boardStates.length === 0) {
            statesDiv.classList.add('hidden');
            return;
        }

        statesDiv.classList.remove('hidden');
        const totalMoves = this.currentGame.boardStates.length;
        stateInfo.textContent = `Move ${this.currentReplayIndex + 1} of ${totalMoves}`;

        // Update replay status
        if (this.replayInterval) {
            replayStatus.textContent = `Playing... Move ${this.currentReplayIndex + 1}`;
        } else if (this.currentGame.status === 'finished') {
            replayStatus.textContent = 'Game finished - Use controls to replay';
        } else {
            replayStatus.textContent = 'Game in progress...';
        }
    }

    showReplayControls() {
        const historyDiv = document.getElementById('moveHistory');
        const statesDiv = document.getElementById('boardStates');

        historyDiv.classList.remove('hidden');
        statesDiv.classList.remove('hidden');
    }

    // Replay functionality
    startReplay() {
        if (!this.currentGame.boardStates || this.currentGame.boardStates.length === 0) return;

        this.pauseReplay();
        this.currentReplayIndex = 0;
        this.renderBoard(this.currentGame.boardStates[0].board);
        this.updateReplayControls();
        this.updateMoveHistory();

        this.replayInterval = setInterval(() => {
            this.currentReplayIndex++;

            if (this.currentReplayIndex >= this.currentGame.boardStates.length) {
                this.pauseReplay();
                this.currentReplayIndex = this.currentGame.boardStates.length - 1;
                return;
            }

            this.renderBoard(this.currentGame.boardStates[this.currentReplayIndex].board);
            this.updateReplayControls();
            this.updateMoveHistory();
        }, 1000); // 1 second per move
    }

    pauseReplay() {
        if (this.replayInterval) {
            clearInterval(this.replayInterval);
            this.replayInterval = null;
            document.getElementById('replayStatus').textContent = 'Paused';
        }
    }

    resetReplay() {
        this.pauseReplay();
        this.currentReplayIndex = this.currentGame.boardStates.length - 1;
        this.renderBoard();
        this.updateReplayControls();
        this.updateMoveHistory();
    }

    previousState() {
        if (!this.currentGame.boardStates || this.currentReplayIndex <= 0) return;

        this.pauseReplay();
        this.currentReplayIndex--;
        this.renderBoard(this.currentGame.boardStates[this.currentReplayIndex].board);
        this.updateReplayControls();
        this.updateMoveHistory();
    }

    nextState() {
        if (!this.currentGame.boardStates || this.currentReplayIndex >= this.currentGame.boardStates.length - 1) return;

        this.pauseReplay();
        this.currentReplayIndex++;
        this.renderBoard(this.currentGame.boardStates[this.currentReplayIndex].board);
        this.updateReplayControls();
        this.updateMoveHistory();
    }

    jumpToState(index) {
        if (!this.currentGame.boardStates || index < 0 || index >= this.currentGame.boardStates.length) return;

        this.pauseReplay();
        this.currentReplayIndex = index;
        this.renderBoard(this.currentGame.boardStates[index].board);
        this.updateReplayControls();
        this.updateMoveHistory();
    }

    canMakeMove() {
        if (!this.currentGame) return false;
        if (this.currentGame.status !== 'playing') return false;
        if (this.currentGame.isAIvsAI) return false; // Can't interact with AI vs AI
        if (!this.currentGame.waitingForHumanInput) return false;

        // Check if current player is human
        const currentPlayer = this.currentGame.currentPlayer;
        const isHuman = (currentPlayer === 'X' && this.currentGame.player1 === 'human') ||
                       (currentPlayer === 'O' && this.currentGame.player2 === 'human');

        return isHuman;
    }

    showGameElements() {
        document.getElementById('gameBoard').classList.remove('hidden');
        document.getElementById('gameStatus').classList.remove('hidden');
    }

    async resetGame() {
        if (this.currentGame) {
            try {
                await fetch(`${this.apiBase}/api/games/${this.currentGame.id}`, {
                    method: 'DELETE'
                });
            } catch (error) {
                console.error('Failed to delete game:', error);
            }
        }

        // Clear polling interval
        if (this.pollInterval) {
            clearInterval(this.pollInterval);
            this.pollInterval = null;
        }

        this.pauseReplay();
        this.currentGame = null;
        this.currentReplayIndex = 0;
        this.createBoard();
        document.getElementById('gameBoard').classList.add('hidden');
        document.getElementById('gameStatus').classList.add('hidden');
        document.getElementById('moveHistory').classList.add('hidden');
        this.hideError();

        const statusDiv = document.getElementById('gameStatus');
        statusDiv.textContent = 'Click "Start New Game" to begin!';
        statusDiv.className = 'game-status status-waiting';
    }

    showError(message) {
        const errorDiv = document.getElementById('error');
        errorDiv.textContent = message;
        errorDiv.classList.remove('hidden');
    }

    hideError() {
        const errorDiv = document.getElementById('error');
        errorDiv.classList.add('hidden');
    }
}

// Initialize the game when the page loads
document.addEventListener('DOMContentLoaded', () => {
    new Connect4GameUI();
});
