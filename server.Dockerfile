# Enhanced Dockerfile with Node.js Frontend
FROM swipl:9.3.35

# Install Node.js
RUN apt-get update && apt-get install -y \
    curl \
    && curl -fsSL https://deb.nodesource.com/setup_18.x | bash - \
    && apt-get install -y nodejs \
    && rm -rf /var/lib/apt/lists/*

# Copy source files
WORKDIR /app
COPY ./src src/
COPY ./src/bootstrap.example.pl src/bootstrap.pl
COPY ./Makefile ./
COPY ./server server/

# Install Node.js dependencies
WORKDIR /app/server
RUN npm install

# Expose port
EXPOSE 3000

# Start the server
CMD ["npm", "start"]
