# Flash Review Backend

This is the backend for the Flash Review application that uses PostgreSQL for data persistence.

## Overview

The Flash Review backend provides a REST API for managing flashcards. It uses the Servant framework and stores data in a PostgreSQL database with the Persistent library.

## Prerequisites

Before running the application, you need:

1. PostgreSQL (version 12 or higher) installed and running
2. The Haskell Stack build tool
3. Create a database named `flashcards`

You have several options to set up PostgreSQL:

### Option 1: Using Docker

If you have Docker installed, you can use the included `docker-compose.yml` file:

```bash
# Start PostgreSQL in a Docker container
docker-compose up -d

# Stop the container
docker-compose down
```

If you encounter issues with Docker, please refer to the [Docker Troubleshooting Guide](./DOCKER-TROUBLESHOOTING.md).

### Option 2: Using the Setup Scripts


### Option 2: Manual Setup

If you already have PostgreSQL installed, create the database manually:

```sql
CREATE DATABASE flashcards;
```

## Configuration

The database connection settings are in `Database.hs`. By default, it connects to:

- Host: localhost
- Database: flashcards
- User: postgres
- Password: postgres
- Port: 5432

You may need to modify these settings to match your PostgreSQL configuration.

## Building and Running

Build the project:

```bash
stack build
```

Run the application:

```bash
stack exec flash-review-backend-exe
```

The server will start on port 8081.

## API Endpoints

The backend provides the following endpoints:

- `GET /cards` - Get all flashcards
- `POST /cards` - Create a new flashcard
- `PUT /cards/:id` - Update a flashcard
- `DELETE /cards/:id` - Delete a flashcard
- `GET /review/queue` - Get cards that are due for review
- `POST /review/:id` - Submit a review for a card
- `GET /stats` - Get statistics about cards due for review

## Flashcard Structure

A flashcard has the following structure:

```json
{
  "id": "UUID",
  "front": "Question text",
  "back": "Answer text",
  "nextReview": "2023-07-06T12:00:00Z",
  "interval": 1,
  "easeFactor": 2.5,
  "repetitions": 0
}
```

## Spaced Repetition Algorithm

This application implements a simplified version of the SM-2 spaced repetition algorithm to schedule flashcard reviews based on user performance.
