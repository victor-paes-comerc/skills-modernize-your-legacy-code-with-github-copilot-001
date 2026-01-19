# Account Management System - Node.js Implementation

This is a Node.js implementation of the legacy COBOL Account Management System. The application preserves all original business logic, data integrity, and menu options from the COBOL version.

## Architecture

The application follows the same layered architecture as the COBOL implementation:

### DataProgram (Data Layer)
- Manages in-memory storage for account balance
- Provides READ and WRITE operations
- Initial balance: 1000.00

### Operations (Business Logic Layer)
- Implements core business logic for all account operations
- View Balance (TOTAL)
- Credit Account (CREDIT)
- Debit Account (DEBIT) with insufficient funds validation

### MainProgram (UI Layer)
- Displays interactive menu
- Accepts user input
- Routes requests to Operations layer
- Controls program execution loop

## Business Rules Preserved

1. **Initial Balance**: All accounts start with 1000.00
2. **No Overdraft**: Debits are rejected if amount exceeds balance
3. **Immediate Processing**: All transactions are processed immediately
4. **Decimal Precision**: All amounts maintain 2 decimal places
5. **Balance Constraints**: 
   - Minimum balance: 0.00
   - Maximum balance: 999,999.99

## Installation

```bash
cd src/accounting
npm install
```

## Running the Application

```bash
npm start
```

Or:

```bash
node index.js
```

## Using VS Code Debugger

1. Open VS Code
2. Navigate to Run and Debug (Ctrl+Shift+D)
3. Select "Launch Account Management System"
4. Press F5 to start debugging

## Menu Options

1. **View Balance** - Display current account balance
2. **Credit Account** - Add funds to the account
3. **Debit Account** - Remove funds from the account (with validation)
4. **Exit** - Close the application

## Testing

### Run Tests

```bash
npm test
```

### Run Tests with Coverage

```bash
npm run test:coverage
```

### Test Statistics

- **Total Tests**: 71 passing
- **Test Categories**: 11
- **Coverage**: 
  - Statements: 54.76%
  - Branches: 78.57%
  - Functions: 60%
  - Lines: 54.76%

### Test Coverage by Category

The test suite covers all business logic from [../../docs/TESTPLAN.md](../../docs/TESTPLAN.md):

1. **DataProgram Tests** (6 tests) - Data persistence layer validation
2. **View Balance Operations** (5 tests) - Balance inquiry functionality
3. **Credit Account Operations** (9 tests) - Credit transaction processing
4. **Debit Account - Sufficient Funds** (6 tests) - Valid debit operations
5. **Debit Account - Insufficient Funds** (5 tests) - Overdraft protection
6. **Complex Transaction Sequences** (9 tests) - Multi-operation workflows
7. **Data Persistence and Consistency** (4 tests) - Data integrity validation
8. **Decimal Precision and Rounding** (6 tests) - Numeric accuracy
9. **Boundary Value Testing** (5 tests) - Edge case handling
10. **User Input Validation** (5 tests) - Input validation and error handling
11. **Operations Execute Dispatcher** (4 tests) - Operation routing
12. **Critical Business Rules** (7 tests) - Core business logic validation

Refer to [../../docs/TESTPLAN.md](../../docs/TESTPLAN.md) for the comprehensive test plan covering all business logic.

## Data Flow

The application follows the same data flow patterns as documented in [../../docs/README.md](../../docs/README.md):

1. User interacts with MainProgram
2. MainProgram calls Operations with operation type
3. Operations reads/writes data through DataProgram
4. Results are displayed to user
5. Loop continues until user exits

## Key Differences from COBOL

While preserving all business logic, the Node.js implementation includes:

- **Asynchronous I/O**: Uses async/await for user input
- **Object-Oriented Design**: Classes instead of procedures
- **Modern Error Handling**: Try-catch blocks and proper error messages
- **Module Exports**: Components can be imported for testing
- **Input Validation**: Enhanced validation for user input
- **Floating-Point Precision**: Proper rounding to avoid JavaScript precision issues

## Files

- `index.js` - Main application file with all three components (DataProgram, Operations, MainProgram)
- `index.test.js` - Comprehensive unit tests (71 test cases)
- `package.json` - Node.js project configuration
- `jest.config.js` - Jest testing framework configuration
- `README.md` - This file

## Future Enhancements

- Add unit and integration tests for MainProgram UI layer
- Implement database persistence (PostgreSQL, MongoDB, etc.)
- Add user authentication and authorization
- Create transaction history and audit trail
- Add REST API endpoints
- Implement concurrent access controls
- Add transaction rollback capabilities
- Create web-based UI
- Add logging and monitoring

