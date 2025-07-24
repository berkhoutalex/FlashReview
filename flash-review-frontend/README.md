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
- `test/` - Test files

## Running Tests

To run the tests:

```bash
npm test
```

This will use Spago to run the test suite, which uses PureScript Spec as the testing framework.

### Test Structure

- `test/Main.purs` - Main test entry point
- `test/API/Types/Spec.purs` - Tests for API data types
- `test/API/Client/Spec.purs` - Tests for API client functions
- `test/Components/Spec.purs` - Tests for UI components

### Testing Approach

The tests use PureScript's Spec testing library. The tests are organized as follows:

1. **API Types Tests**: These test the JSON encoding and decoding of the application's data types.
2. **API Client Tests**: These test the API client functions with mock fetch responses.
3. **Components Tests**: These contain placeholder tests for Halogen components.

#### Required Dependencies

For running the tests, ensure you have these dependencies:
- `purescript-spec`: For the testing framework
- `purescript-datetime`: For date/time operations
- `purescript-formatters`: For parsing date strings
- `purescript-uuid`: For UUID operations

#### Working with UUIDs and DateTimes in Tests

The test suite includes helper functions for parsing string representations of UUIDs and DateTimes:
- `parseUUID`: Converts string to UUID type
- `parseDateTime`: Converts string to DateTime type

To extend the component tests, you'll need to add a proper Halogen testing library or create a custom testing harness. The current implementation focuses on testing the data layer of the application.
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
