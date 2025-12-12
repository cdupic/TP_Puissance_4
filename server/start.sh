#!/bin/bash

# Enhanced Connect4 Web Server Startup Script with Board History

echo "🚀 Starting Enhanced Connect4 Web Server with Board History..."
echo "✨ New Features: Move-by-move replay, AI vs AI game watching, board state history"
echo ""

# Check prerequisites
if ! command -v node &> /dev/null; then
    echo "❌ Node.js is not installed. Please install Node.js 18+ first."
    exit 1
fi

if ! command -v swipl &> /dev/null; then
    echo "❌ SWI-Prolog is not installed. Please install SWI-Prolog first."
    exit 1
fi

# Install dependencies if needed
if [ ! -d "node_modules" ]; then
    echo "📦 Installing dependencies..."
    npm install
fi

# Set environment variables
export PORT=${PORT:-3000}
export NODE_ENV=${NODE_ENV:-development}

echo "🌟 Enhanced Features Available:"
echo "   • Real-time AI vs AI game watching"
echo "   • Move-by-move replay functionality"
echo "   • Complete board state history"
echo "   • Interactive game navigation"
echo ""
echo "🌐 Server will be available at: http://localhost:$PORT"
echo "🎮 Web interface: http://localhost:$PORT"
echo "📡 API endpoints: http://localhost:$PORT/api/games"
echo ""
echo "🎯 Try these scenarios:"
echo "   1. AI vs AI: Select 'Minimax' vs 'Random' and watch the game unfold!"
echo "   2. Replay: After any game, use the replay controls to see move-by-move"
echo "   3. Human vs AI: Play against different AI difficulty levels"
echo ""
echo "Press Ctrl+C to stop the server"
echo ""

# Start the server
npm start
