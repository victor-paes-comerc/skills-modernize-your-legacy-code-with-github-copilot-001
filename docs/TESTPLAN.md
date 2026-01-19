# Account Management System - Test Plan

## Purpose
This test plan validates the business logic and implementation of the Account Management System. It will be used to ensure consistency when migrating from COBOL to Node.js.

## Scope
This test plan covers all business logic, user interactions, data validation, and error handling in the current COBOL application.

## Test Environment
- Initial account balance: 1,000.00
- Balance format: 9(6)V99 (max: 999,999.99)
- Menu options: 1-4

---

## Test Cases

### 1. Menu Display and Navigation

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|---------|----------|
| TC-001 | Verify menu displays correctly on startup | Application started | 1. Launch application | Menu displays with options 1-4:<br>- View Balance<br>- Credit Account<br>- Debit Account<br>- Exit | | | |
| TC-002 | Select valid menu option 1 | Menu displayed | 1. Enter choice "1" | System accepts input and proceeds to View Balance operation | | | |
| TC-003 | Select valid menu option 2 | Menu displayed | 1. Enter choice "2" | System accepts input and proceeds to Credit Account operation | | | |
| TC-004 | Select valid menu option 3 | Menu displayed | 1. Enter choice "3" | System accepts input and proceeds to Debit Account operation | | | |
| TC-005 | Select valid menu option 4 | Menu displayed | 1. Enter choice "4" | System displays "Exiting the program. Goodbye!" and terminates | | | |
| TC-006 | Enter invalid menu choice (0) | Menu displayed | 1. Enter choice "0" | System displays "Invalid choice, please select 1-4."<br>2. Menu redisplays | | | |
| TC-007 | Enter invalid menu choice (5) | Menu displayed | 1. Enter choice "5" | System displays "Invalid choice, please select 1-4."<br>2. Menu redisplays | | | |
| TC-008 | Enter invalid menu choice (9) | Menu displayed | 1. Enter choice "9" | System displays "Invalid choice, please select 1-4."<br>2. Menu redisplays | | | |
| TC-009 | Menu redisplays after operation | Any operation completed | 1. Complete any operation (1, 2, or 3) | Menu redisplays automatically after operation completes | | | |

---

### 2. View Balance Operations

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|---------|----------|
| TC-010 | View initial balance | Application started, no transactions | 1. Select option 1 | Display shows "Current balance: 1000.00" | | | |
| TC-011 | View balance after credit | Balance = 1000.00, credited 500.00 | 1. Select option 1 | Display shows "Current balance: 1500.00" | | | |
| TC-012 | View balance after debit | Balance = 1000.00, debited 300.00 | 1. Select option 1 | Display shows "Current balance: 700.00" | | | |
| TC-013 | View balance after multiple transactions | Multiple credits and debits performed | 1. Select option 1 | Display shows correct accumulated balance | | | |
| TC-014 | View balance with decimal precision | Balance has decimal values | 1. Select option 1 | Display shows balance with 2 decimal places | | | |

---

### 3. Credit Account Operations

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|---------|----------|
| TC-015 | Credit account with valid amount | Balance = 1000.00 | 1. Select option 2<br>2. Enter amount: 500.00 | System displays "Amount credited. New balance: 1500.00" | | | |
| TC-016 | Credit account with small amount | Balance = 1000.00 | 1. Select option 2<br>2. Enter amount: 0.01 | System displays "Amount credited. New balance: 1000.01" | | | |
| TC-017 | Credit account with large amount | Balance = 1000.00 | 1. Select option 2<br>2. Enter amount: 50000.00 | System displays "Amount credited. New balance: 51000.00" | | | |
| TC-018 | Credit account with decimal amount | Balance = 1000.00 | 1. Select option 2<br>2. Enter amount: 123.45 | System displays "Amount credited. New balance: 1123.45" | | | |
| TC-019 | Credit account with whole number | Balance = 1000.00 | 1. Select option 2<br>2. Enter amount: 100 | System displays "Amount credited. New balance: 1100.00" | | | |
| TC-020 | Credit account near maximum balance | Balance = 998999.99 | 1. Select option 2<br>2. Enter amount: 1000.00 | System displays "Amount credited. New balance: 999999.99" | | | Maximum balance reached |
| TC-021 | Multiple consecutive credits | Balance = 1000.00 | 1. Select option 2, credit 100<br>2. Select option 2, credit 200<br>3. Select option 2, credit 300 | Final balance = 1600.00 | | | |
| TC-022 | Credit with zero amount | Balance = 1000.00 | 1. Select option 2<br>2. Enter amount: 0 | System displays "Amount credited. New balance: 1000.00" | | | Edge case |
| TC-023 | Credit with maximum valid amount | Balance = 1000.00 | 1. Select option 2<br>2. Enter amount: 999999.99 | System attempts to credit (may exceed max balance) | | | Should verify system behavior |

---

### 4. Debit Account Operations - Sufficient Funds

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|---------|----------|
| TC-024 | Debit account with sufficient funds | Balance = 1000.00 | 1. Select option 3<br>2. Enter amount: 500.00 | System displays "Amount debited. New balance: 500.00" | | | |
| TC-025 | Debit small amount | Balance = 1000.00 | 1. Select option 3<br>2. Enter amount: 0.01 | System displays "Amount debited. New balance: 999.99" | | | |
| TC-026 | Debit with decimal amount | Balance = 1000.00 | 1. Select option 3<br>2. Enter amount: 123.45 | System displays "Amount debited. New balance: 876.55" | | | |
| TC-027 | Debit exact balance amount | Balance = 1000.00 | 1. Select option 3<br>2. Enter amount: 1000.00 | System displays "Amount debited. New balance: 0.00" | | | Boundary condition |
| TC-028 | Debit leaving small balance | Balance = 1000.00 | 1. Select option 3<br>2. Enter amount: 999.99 | System displays "Amount debited. New balance: 0.01" | | | |
| TC-029 | Multiple consecutive debits | Balance = 1000.00 | 1. Select option 3, debit 100<br>2. Select option 3, debit 200<br>3. Select option 3, debit 300 | Final balance = 400.00 | | | |
| TC-030 | Debit from zero balance | Balance = 0.00 | 1. Select option 3<br>2. Enter amount: 0.00 | System displays "Amount debited. New balance: 0.00" | | | Edge case |

---

### 5. Debit Account Operations - Insufficient Funds

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|---------|----------|
| TC-031 | Debit more than balance | Balance = 1000.00 | 1. Select option 3<br>2. Enter amount: 1500.00 | System displays "Insufficient funds for this debit."<br>Balance remains 1000.00 | | | Critical business rule |
| TC-032 | Debit exceeding balance by 0.01 | Balance = 1000.00 | 1. Select option 3<br>2. Enter amount: 1000.01 | System displays "Insufficient funds for this debit."<br>Balance remains 1000.00 | | | Boundary condition |
| TC-033 | Debit from zero balance (non-zero amount) | Balance = 0.00 | 1. Select option 3<br>2. Enter amount: 0.01 | System displays "Insufficient funds for this debit."<br>Balance remains 0.00 | | | |
| TC-034 | Debit large amount with insufficient funds | Balance = 100.00 | 1. Select option 3<br>2. Enter amount: 10000.00 | System displays "Insufficient funds for this debit."<br>Balance remains 100.00 | | | |
| TC-035 | Verify balance unchanged after insufficient funds | Balance = 500.00 | 1. Select option 3, enter 600<br>2. Select option 1 | Balance displays as 500.00 | | | Data integrity check |

---

### 6. Complex Transaction Sequences

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|---------|----------|
| TC-036 | Credit then debit sequence | Balance = 1000.00 | 1. Credit 500.00<br>2. Debit 300.00<br>3. View balance | Balance = 1200.00 | | | |
| TC-037 | Debit then credit sequence | Balance = 1000.00 | 1. Debit 400.00<br>2. Credit 200.00<br>3. View balance | Balance = 800.00 | | | |
| TC-038 | Multiple operations ending at zero | Balance = 1000.00 | 1. Credit 500.00<br>2. Debit 1500.00<br>3. View balance | Balance = 0.00 | | | |
| TC-039 | Failed debit followed by successful operation | Balance = 100.00 | 1. Debit 200.00 (fails)<br>2. Credit 50.00<br>3. View balance | Balance = 150.00 | | | Verify first operation doesn't affect subsequent operations |
| TC-040 | Alternating credits and debits | Balance = 1000.00 | 1. Credit 100<br>2. Debit 50<br>3. Credit 200<br>4. Debit 150<br>5. View balance | Balance = 1100.00 | | | |
| TC-041 | Bring balance to zero then credit | Balance = 500.00 | 1. Debit 500.00<br>2. Credit 1000.00<br>3. View balance | Balance = 1000.00 | | | |
| TC-042 | Bring balance to zero then attempted debit | Balance = 100.00 | 1. Debit 100.00<br>2. Debit 50.00 (should fail)<br>3. View balance | Balance = 0.00, second debit rejected | | | |

---

### 7. Data Persistence and Consistency

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|---------|----------|
| TC-043 | Balance persists across multiple view operations | Balance modified to 1234.56 | 1. View balance<br>2. View balance again<br>3. View balance again | All three displays show 1234.56 | | | |
| TC-044 | Balance persists between operations | Balance = 1000.00 | 1. Credit 100<br>2. View balance<br>3. Debit 50<br>4. View balance | First view shows 1100.00<br>Second view shows 1050.00 | | | |
| TC-045 | Failed operation doesn't modify balance | Balance = 500.00 | 1. View balance<br>2. Debit 600 (fails)<br>3. View balance | Both views show 500.00 | | | Critical - data integrity |
| TC-046 | Balance calculation accuracy | Balance = 1000.00 | 1. Credit 333.33<br>2. Credit 333.33<br>3. Credit 333.34<br>4. View balance | Balance = 2000.00 | | | Decimal precision test |
| TC-047 | Large number of transactions | Balance = 1000.00 | 1. Perform 10 credits of 100<br>2. Perform 10 debits of 50<br>3. View balance | Balance = 1500.00 | | | Stress test |

---

### 8. Decimal Precision and Rounding

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|---------|----------|
| TC-048 | Credit with two decimal places | Balance = 1000.00 | 1. Credit 123.45 | Balance = 1123.45 | | | |
| TC-049 | Debit with two decimal places | Balance = 1000.00 | 1. Debit 234.56 | Balance = 765.44 | | | |
| TC-050 | Multiple decimal operations | Balance = 1000.00 | 1. Credit 0.01<br>2. Credit 0.01<br>3. Debit 0.01 | Balance = 1000.01 | | | Precision test |
| TC-051 | Operations resulting in .99 balance | Balance = 1000.00 | 1. Debit 0.01 | Balance = 999.99 | | | |
| TC-052 | Verify two decimal place display | Balance = 1000.00 | 1. Credit 100<br>2. View balance | Display shows "1100.00" with 2 decimal places | | | Format verification |

---

### 9. Boundary Value Testing

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|---------|----------|
| TC-053 | Minimum balance (zero) | Balance reduced to 0.00 | 1. View balance | Display shows "Current balance: 0.00" | | | |
| TC-054 | Maximum balance | Balance increased to 999999.99 | 1. View balance | Display shows "Current balance: 999999.99" | | | |
| TC-055 | Credit minimum amount (0.01) | Balance = 0.00 | 1. Credit 0.01 | Balance = 0.01 | | | |
| TC-056 | Debit minimum amount (0.01) | Balance = 0.01 | 1. Debit 0.01 | Balance = 0.00 | | | |
| TC-057 | Operation at maximum balance limit | Balance = 999999.99 | 1. Credit 0.01 | System behavior when exceeding max | | | May overflow or error |
| TC-058 | Exact balance debit (boundary) | Balance = 555.55 | 1. Debit 555.55 | Balance = 0.00 | | | |

---

### 10. User Input Validation

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|---------|----------|
| TC-059 | Menu accepts single digit 1-4 | Menu displayed | 1. Enter values 1, 2, 3, 4 sequentially | Each input accepted and processed | | | |
| TC-060 | Menu rejects out of range values | Menu displayed | 1. Enter 0<br>2. Enter 5-9 | Error message displayed, menu redisplays | | | |
| TC-061 | Amount input accepts decimal values | Credit operation initiated | 1. Enter 123.45 | Amount accepted and processed | | | |
| TC-062 | Amount input accepts whole numbers | Credit operation initiated | 1. Enter 100 | Amount accepted, processed as 100.00 | | | |

---

### 11. Program Flow and Exit

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|---------|----------|
| TC-063 | Exit program immediately | Application started | 1. Select option 4 | Program displays goodbye message and exits | | | |
| TC-064 | Exit program after operations | Multiple operations performed | 1. Perform 2-3 operations<br>2. Select option 4 | Program displays goodbye message and exits | | | |
| TC-065 | Program loop continues until exit | Application started | 1. Perform 5+ operations<br>2. Select option 4 | Program continues looping, only exits when option 4 selected | | | |
| TC-066 | Program loop after invalid input | Menu displayed | 1. Enter invalid choice<br>2. Verify menu redisplays | Menu redisplays, program continues | | | |

---

## Test Execution Summary

| Category | Total Tests | Passed | Failed | Blocked | Not Executed |
|----------|-------------|--------|--------|---------|--------------|
| Menu Display and Navigation | 9 | | | | |
| View Balance Operations | 5 | | | | |
| Credit Account Operations | 9 | | | | |
| Debit Account - Sufficient Funds | 7 | | | | |
| Debit Account - Insufficient Funds | 5 | | | | |
| Complex Transaction Sequences | 7 | | | | |
| Data Persistence and Consistency | 5 | | | | |
| Decimal Precision and Rounding | 5 | | | | |
| Boundary Value Testing | 6 | | | | |
| User Input Validation | 4 | | | | |
| Program Flow and Exit | 4 | | | | |
| **TOTAL** | **66** | **0** | **0** | **0** | **66** |

---

## Critical Business Rules to Validate

1. **No Overdraft:** System must reject debits when amount exceeds balance
2. **Balance Persistence:** Balance must remain consistent across operations
3. **Decimal Precision:** All amounts must maintain 2 decimal places
4. **Transaction Atomicity:** Failed operations must not modify balance
5. **Data Integrity:** Balance must never become negative
6. **Initial Balance:** New accounts start with 1000.00
7. **Maximum Balance:** System must handle balance up to 999,999.99

---

## Notes for Node.js Implementation

### Test Automation Recommendations
- All test cases should be automated using Jest or Mocha
- Create separate test suites for unit tests (individual functions) and integration tests (complete flows)
- Mock data persistence layer for unit tests
- Use actual data persistence for integration tests

### Test Data Management
- Reset balance to 1000.00 before each test
- Create test fixtures for common scenarios
- Consider parameterized tests for boundary values

### Additional Testing for Node.js
- API endpoint testing (if REST API)
- Concurrent transaction handling
- Database transaction rollback scenarios
- Input sanitization and validation
- Error handling and logging

### Performance Testing
- Response time for each operation
- Concurrent user handling
- Database query optimization
- Memory usage during extended operations

---

## Sign-off

| Role | Name | Signature | Date |
|------|------|-----------|------|
| Business Stakeholder | | | |
| QA Lead | | | |
| Development Lead | | | |
| Project Manager | | | |

---

## Revision History

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0 | 2026-01-19 | System | Initial test plan creation from COBOL analysis |
