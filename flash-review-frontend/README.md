# Flash Review Frontend

This is the PureScript frontend for the Flash Review application, a flashcard application using spaced repetition.

## Prerequisites

- Node.js (v14.x or higher)
- npm (v6.x or higher)
- PureScript (v0.15.x or higher)
- Spago (v0.20.x or higher)

## Installation

1. Clone the repository and navigate to the project directory:
   ```bash
   cd flash-review-frontend
   ```

2. Install dependencies:
   ```bash
   npm install
   ```

## Running the Application

1. Build the PureScript code:
   ```bash
   npm run build
   ```

2. Bundle the application:
   ```bash
   npm run bundle
   ```

3. Start the development server:
   ```bash
   npm run serve
   ```

4. Or do all of the above in one command:
   ```bash
   npm start
   ```

5. Open your browser and navigate to `http://localhost:3000`

## Project Structure

- `src/` - PureScript source files
  - `API/` - API client modules for communicating with the backend
  - `Components/` - Halogen UI components
  - `Main.purs` - Application entry point

## API Communication

The application communicates with the Haskell backend server running on `localhost:8081`. Make sure the backend server is running before using the application.

The API client provides the following operations:

- Get all flashcards
- Create a new flashcard
- Update an existing flashcard
- Delete a flashcard
- Get the review queue
- Submit review results
- Get statistics
