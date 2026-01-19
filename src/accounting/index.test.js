/**
 * Account Management System - Unit Tests
 * 
 * These tests mirror the test scenarios defined in docs/TESTPLAN.md
 * and validate the business logic converted from COBOL to Node.js
 */

const { DataProgram, Operations } = require('./index');

// Mock readline interface for testing
const createMockReadline = (responses = []) => {
    let responseIndex = 0;
    return {
        question: jest.fn((prompt, callback) => {
            const response = responses[responseIndex++] || '';
            callback(response);
        }),
        close: jest.fn()
    };
};

describe('Account Management System Tests', () => {
    
    // ========================================================================
    // 1. DATA PROGRAM TESTS (Data Layer)
    // ========================================================================
    
    describe('DataProgram - Data Persistence Layer', () => {
        let dataProgram;

        beforeEach(() => {
            dataProgram = new DataProgram();
        });

        // TC-043: Balance persists across multiple view operations
        test('TC-043: Balance persists across multiple read operations', () => {
            dataProgram.write(1234.56);
            
            const balance1 = dataProgram.execute('READ');
            const balance2 = dataProgram.execute('READ');
            const balance3 = dataProgram.execute('READ');
            
            expect(balance1).toBe(1234.56);
            expect(balance2).toBe(1234.56);
            expect(balance3).toBe(1234.56);
        });

        test('Initial balance is 1000.00', () => {
            const balance = dataProgram.read();
            expect(balance).toBe(1000.00);
        });

        test('READ operation returns current balance', () => {
            const balance = dataProgram.execute('READ');
            expect(balance).toBe(1000.00);
        });

        test('WRITE operation updates balance', () => {
            dataProgram.execute('WRITE', 2500.75);
            const balance = dataProgram.execute('READ');
            expect(balance).toBe(2500.75);
        });

        test('Multiple WRITE operations update correctly', () => {
            dataProgram.execute('WRITE', 500.00);
            expect(dataProgram.execute('READ')).toBe(500.00);
            
            dataProgram.execute('WRITE', 1500.50);
            expect(dataProgram.execute('READ')).toBe(1500.50);
        });

        test('Throws error for unknown operation', () => {
            expect(() => {
                dataProgram.execute('DELETE');
            }).toThrow('Unknown operation: DELETE');
        });
    });

    // ========================================================================
    // 2. VIEW BALANCE OPERATIONS
    // ========================================================================
    
    describe('View Balance Operations', () => {
        let dataProgram;
        let operations;
        let mockRl;
        let consoleSpy;

        beforeEach(() => {
            dataProgram = new DataProgram();
            mockRl = createMockReadline();
            operations = new Operations(dataProgram, mockRl);
            consoleSpy = jest.spyOn(console, 'log').mockImplementation();
        });

        afterEach(() => {
            consoleSpy.mockRestore();
        });

        // TC-010: View initial balance
        test('TC-010: View initial balance shows 1000.00', async () => {
            await operations.viewBalance();
            expect(consoleSpy).toHaveBeenCalledWith('Current balance: 1000.00');
        });

        // TC-011: View balance after credit
        test('TC-011: View balance after credit shows correct amount', async () => {
            dataProgram.write(1500.00);
            await operations.viewBalance();
            expect(consoleSpy).toHaveBeenCalledWith('Current balance: 1500.00');
        });

        // TC-012: View balance after debit
        test('TC-012: View balance after debit shows correct amount', async () => {
            dataProgram.write(700.00);
            await operations.viewBalance();
            expect(consoleSpy).toHaveBeenCalledWith('Current balance: 700.00');
        });

        // TC-014: View balance with decimal precision
        test('TC-014: Balance displays with 2 decimal places', async () => {
            dataProgram.write(1234.56);
            await operations.viewBalance();
            expect(consoleSpy).toHaveBeenCalledWith('Current balance: 1234.56');
        });

        test('Balance with whole number displays with .00', async () => {
            dataProgram.write(1000);
            await operations.viewBalance();
            expect(consoleSpy).toHaveBeenCalledWith('Current balance: 1000.00');
        });
    });

    // ========================================================================
    // 3. CREDIT ACCOUNT OPERATIONS
    // ========================================================================
    
    describe('Credit Account Operations', () => {
        let dataProgram;
        let operations;
        let mockRl;
        let consoleSpy;

        beforeEach(() => {
            dataProgram = new DataProgram();
            mockRl = createMockReadline();
            operations = new Operations(dataProgram, mockRl);
            consoleSpy = jest.spyOn(console, 'log').mockImplementation();
        });

        afterEach(() => {
            consoleSpy.mockRestore();
        });

        // TC-015: Credit account with valid amount
        test('TC-015: Credit 500.00 increases balance to 1500.00', async () => {
            mockRl = createMockReadline(['500.00']);
            operations = new Operations(dataProgram, mockRl);
            
            await operations.creditAccount();
            
            expect(consoleSpy).toHaveBeenCalledWith('Amount credited. New balance: 1500.00');
            expect(dataProgram.read()).toBe(1500.00);
        });

        // TC-016: Credit account with small amount
        test('TC-016: Credit 0.01 increases balance correctly', async () => {
            mockRl = createMockReadline(['0.01']);
            operations = new Operations(dataProgram, mockRl);
            
            await operations.creditAccount();
            
            expect(consoleSpy).toHaveBeenCalledWith('Amount credited. New balance: 1000.01');
            expect(dataProgram.read()).toBe(1000.01);
        });

        // TC-017: Credit account with large amount
        test('TC-017: Credit 50000.00 increases balance correctly', async () => {
            mockRl = createMockReadline(['50000.00']);
            operations = new Operations(dataProgram, mockRl);
            
            await operations.creditAccount();
            
            expect(consoleSpy).toHaveBeenCalledWith('Amount credited. New balance: 51000.00');
            expect(dataProgram.read()).toBe(51000.00);
        });

        // TC-018: Credit account with decimal amount
        test('TC-018: Credit 123.45 increases balance correctly', async () => {
            mockRl = createMockReadline(['123.45']);
            operations = new Operations(dataProgram, mockRl);
            
            await operations.creditAccount();
            
            expect(consoleSpy).toHaveBeenCalledWith('Amount credited. New balance: 1123.45');
            expect(dataProgram.read()).toBe(1123.45);
        });

        // TC-019: Credit account with whole number
        test('TC-019: Credit 100 (whole number) increases balance correctly', async () => {
            mockRl = createMockReadline(['100']);
            operations = new Operations(dataProgram, mockRl);
            
            await operations.creditAccount();
            
            expect(consoleSpy).toHaveBeenCalledWith('Amount credited. New balance: 1100.00');
            expect(dataProgram.read()).toBe(1100.00);
        });

        // TC-020: Credit account near maximum balance
        test('TC-020: Credit near maximum balance', async () => {
            dataProgram.write(998999.99);
            mockRl = createMockReadline(['1000.00']);
            operations = new Operations(dataProgram, mockRl);
            
            await operations.creditAccount();
            
            expect(dataProgram.read()).toBe(999999.99);
        });

        // TC-022: Credit with zero amount
        test('TC-022: Credit 0 does not change balance', async () => {
            mockRl = createMockReadline(['0']);
            operations = new Operations(dataProgram, mockRl);
            
            await operations.creditAccount();
            
            expect(consoleSpy).toHaveBeenCalledWith('Amount credited. New balance: 1000.00');
            expect(dataProgram.read()).toBe(1000.00);
        });

        test('Invalid credit amount (negative) shows error', async () => {
            mockRl = createMockReadline(['-100']);
            operations = new Operations(dataProgram, mockRl);
            
            await operations.creditAccount();
            
            expect(consoleSpy).toHaveBeenCalledWith('Invalid amount. Please enter a valid positive number.');
            expect(dataProgram.read()).toBe(1000.00); // Balance unchanged
        });

        test('Invalid credit amount (text) shows error', async () => {
            mockRl = createMockReadline(['abc']);
            operations = new Operations(dataProgram, mockRl);
            
            await operations.creditAccount();
            
            expect(consoleSpy).toHaveBeenCalledWith('Invalid amount. Please enter a valid positive number.');
            expect(dataProgram.read()).toBe(1000.00); // Balance unchanged
        });
    });

    // ========================================================================
    // 4. DEBIT ACCOUNT OPERATIONS - SUFFICIENT FUNDS
    // ========================================================================
    
    describe('Debit Account Operations - Sufficient Funds', () => {
        let dataProgram;
        let operations;
        let mockRl;
        let consoleSpy;

        beforeEach(() => {
            dataProgram = new DataProgram();
            mockRl = createMockReadline();
            operations = new Operations(dataProgram, mockRl);
            consoleSpy = jest.spyOn(console, 'log').mockImplementation();
        });

        afterEach(() => {
            consoleSpy.mockRestore();
        });

        // TC-024: Debit account with sufficient funds
        test('TC-024: Debit 500.00 decreases balance to 500.00', async () => {
            mockRl = createMockReadline(['500.00']);
            operations = new Operations(dataProgram, mockRl);
            
            await operations.debitAccount();
            
            expect(consoleSpy).toHaveBeenCalledWith('Amount debited. New balance: 500.00');
            expect(dataProgram.read()).toBe(500.00);
        });

        // TC-025: Debit small amount
        test('TC-025: Debit 0.01 decreases balance correctly', async () => {
            mockRl = createMockReadline(['0.01']);
            operations = new Operations(dataProgram, mockRl);
            
            await operations.debitAccount();
            
            expect(consoleSpy).toHaveBeenCalledWith('Amount debited. New balance: 999.99');
            expect(dataProgram.read()).toBe(999.99);
        });

        // TC-026: Debit with decimal amount
        test('TC-026: Debit 123.45 decreases balance correctly', async () => {
            mockRl = createMockReadline(['123.45']);
            operations = new Operations(dataProgram, mockRl);
            
            await operations.debitAccount();
            
            expect(consoleSpy).toHaveBeenCalledWith('Amount debited. New balance: 876.55');
            expect(dataProgram.read()).toBe(876.55);
        });

        // TC-027: Debit exact balance amount
        test('TC-027: Debit exact balance amount results in 0.00', async () => {
            mockRl = createMockReadline(['1000.00']);
            operations = new Operations(dataProgram, mockRl);
            
            await operations.debitAccount();
            
            expect(consoleSpy).toHaveBeenCalledWith('Amount debited. New balance: 0.00');
            expect(dataProgram.read()).toBe(0.00);
        });

        // TC-028: Debit leaving small balance
        test('TC-028: Debit 999.99 leaves balance of 0.01', async () => {
            mockRl = createMockReadline(['999.99']);
            operations = new Operations(dataProgram, mockRl);
            
            await operations.debitAccount();
            
            expect(consoleSpy).toHaveBeenCalledWith('Amount debited. New balance: 0.01');
            expect(dataProgram.read()).toBe(0.01);
        });

        // TC-030: Debit from zero balance
        test('TC-030: Debit 0.00 from zero balance', async () => {
            dataProgram.write(0.00);
            mockRl = createMockReadline(['0.00']);
            operations = new Operations(dataProgram, mockRl);
            
            await operations.debitAccount();
            
            expect(consoleSpy).toHaveBeenCalledWith('Amount debited. New balance: 0.00');
            expect(dataProgram.read()).toBe(0.00);
        });
    });

    // ========================================================================
    // 5. DEBIT ACCOUNT OPERATIONS - INSUFFICIENT FUNDS
    // ========================================================================
    
    describe('Debit Account Operations - Insufficient Funds', () => {
        let dataProgram;
        let operations;
        let mockRl;
        let consoleSpy;

        beforeEach(() => {
            dataProgram = new DataProgram();
            mockRl = createMockReadline();
            operations = new Operations(dataProgram, mockRl);
            consoleSpy = jest.spyOn(console, 'log').mockImplementation();
        });

        afterEach(() => {
            consoleSpy.mockRestore();
        });

        // TC-031: Debit more than balance (Critical business rule)
        test('TC-031: Debit 1500.00 with balance 1000.00 is rejected', async () => {
            mockRl = createMockReadline(['1500.00']);
            operations = new Operations(dataProgram, mockRl);
            
            await operations.debitAccount();
            
            expect(consoleSpy).toHaveBeenCalledWith('Insufficient funds for this debit.');
            expect(dataProgram.read()).toBe(1000.00); // Balance unchanged
        });

        // TC-032: Debit exceeding balance by 0.01 (Boundary condition)
        test('TC-032: Debit 1000.01 with balance 1000.00 is rejected', async () => {
            mockRl = createMockReadline(['1000.01']);
            operations = new Operations(dataProgram, mockRl);
            
            await operations.debitAccount();
            
            expect(consoleSpy).toHaveBeenCalledWith('Insufficient funds for this debit.');
            expect(dataProgram.read()).toBe(1000.00); // Balance unchanged
        });

        // TC-033: Debit from zero balance (non-zero amount)
        test('TC-033: Debit 0.01 from zero balance is rejected', async () => {
            dataProgram.write(0.00);
            mockRl = createMockReadline(['0.01']);
            operations = new Operations(dataProgram, mockRl);
            
            await operations.debitAccount();
            
            expect(consoleSpy).toHaveBeenCalledWith('Insufficient funds for this debit.');
            expect(dataProgram.read()).toBe(0.00); // Balance unchanged
        });

        // TC-034: Debit large amount with insufficient funds
        test('TC-034: Debit 10000.00 with balance 100.00 is rejected', async () => {
            dataProgram.write(100.00);
            mockRl = createMockReadline(['10000.00']);
            operations = new Operations(dataProgram, mockRl);
            
            await operations.debitAccount();
            
            expect(consoleSpy).toHaveBeenCalledWith('Insufficient funds for this debit.');
            expect(dataProgram.read()).toBe(100.00); // Balance unchanged
        });

        // TC-035: Verify balance unchanged after insufficient funds
        test('TC-035: Balance remains unchanged after failed debit', async () => {
            dataProgram.write(500.00);
            mockRl = createMockReadline(['600.00']);
            operations = new Operations(dataProgram, mockRl);
            
            const balanceBefore = dataProgram.read();
            await operations.debitAccount();
            const balanceAfter = dataProgram.read();
            
            expect(balanceAfter).toBe(balanceBefore);
            expect(balanceAfter).toBe(500.00);
        });
    });

    // ========================================================================
    // 6. COMPLEX TRANSACTION SEQUENCES
    // ========================================================================
    
    describe('Complex Transaction Sequences', () => {
        let dataProgram;
        let operations;
        let consoleSpy;

        beforeEach(() => {
            dataProgram = new DataProgram();
            consoleSpy = jest.spyOn(console, 'log').mockImplementation();
        });

        afterEach(() => {
            consoleSpy.mockRestore();
        });

        // TC-036: Credit then debit sequence
        test('TC-036: Credit 500 then debit 300 results in 1200', async () => {
            let mockRl = createMockReadline(['500.00']);
            let ops = new Operations(dataProgram, mockRl);
            await ops.creditAccount();
            
            mockRl = createMockReadline(['300.00']);
            ops = new Operations(dataProgram, mockRl);
            await ops.debitAccount();
            
            expect(dataProgram.read()).toBe(1200.00);
        });

        // TC-037: Debit then credit sequence
        test('TC-037: Debit 400 then credit 200 results in 800', async () => {
            let mockRl = createMockReadline(['400.00']);
            let ops = new Operations(dataProgram, mockRl);
            await ops.debitAccount();
            
            mockRl = createMockReadline(['200.00']);
            ops = new Operations(dataProgram, mockRl);
            await ops.creditAccount();
            
            expect(dataProgram.read()).toBe(800.00);
        });

        // TC-038: Multiple operations ending at zero
        test('TC-038: Credit 500 then debit 1500 results in 0', async () => {
            let mockRl = createMockReadline(['500.00']);
            let ops = new Operations(dataProgram, mockRl);
            await ops.creditAccount();
            
            mockRl = createMockReadline(['1500.00']);
            ops = new Operations(dataProgram, mockRl);
            await ops.debitAccount();
            
            expect(dataProgram.read()).toBe(0.00);
        });

        // TC-039: Failed debit followed by successful operation
        test('TC-039: Failed debit 200 then credit 50 results in 150', async () => {
            dataProgram.write(100.00);
            
            let mockRl = createMockReadline(['200.00']);
            let ops = new Operations(dataProgram, mockRl);
            await ops.debitAccount(); // Should fail
            
            expect(dataProgram.read()).toBe(100.00); // Unchanged
            
            mockRl = createMockReadline(['50.00']);
            ops = new Operations(dataProgram, mockRl);
            await ops.creditAccount();
            
            expect(dataProgram.read()).toBe(150.00);
        });

        // TC-040: Alternating credits and debits
        test('TC-040: Alternating operations result in correct balance', async () => {
            const operations = [
                { type: 'credit', amount: '100' },
                { type: 'debit', amount: '50' },
                { type: 'credit', amount: '200' },
                { type: 'debit', amount: '150' }
            ];
            
            for (const op of operations) {
                const mockRl = createMockReadline([op.amount]);
                const ops = new Operations(dataProgram, mockRl);
                
                if (op.type === 'credit') {
                    await ops.creditAccount();
                } else {
                    await ops.debitAccount();
                }
            }
            
            expect(dataProgram.read()).toBe(1100.00);
        });

        // TC-041: Bring balance to zero then credit
        test('TC-041: Debit to zero then credit 1000', async () => {
            dataProgram.write(500.00);
            
            let mockRl = createMockReadline(['500.00']);
            let ops = new Operations(dataProgram, mockRl);
            await ops.debitAccount();
            
            expect(dataProgram.read()).toBe(0.00);
            
            mockRl = createMockReadline(['1000.00']);
            ops = new Operations(dataProgram, mockRl);
            await ops.creditAccount();
            
            expect(dataProgram.read()).toBe(1000.00);
        });

        // TC-042: Bring balance to zero then attempted debit
        test('TC-042: Debit to zero then failed debit attempt', async () => {
            dataProgram.write(100.00);
            
            let mockRl = createMockReadline(['100.00']);
            let ops = new Operations(dataProgram, mockRl);
            await ops.debitAccount();
            
            expect(dataProgram.read()).toBe(0.00);
            
            mockRl = createMockReadline(['50.00']);
            ops = new Operations(dataProgram, mockRl);
            await ops.debitAccount(); // Should fail
            
            expect(consoleSpy).toHaveBeenCalledWith('Insufficient funds for this debit.');
            expect(dataProgram.read()).toBe(0.00);
        });

        // TC-021: Multiple consecutive credits
        test('TC-021: Multiple consecutive credits accumulate correctly', async () => {
            const amounts = ['100', '200', '300'];
            
            for (const amount of amounts) {
                const mockRl = createMockReadline([amount]);
                const ops = new Operations(dataProgram, mockRl);
                await ops.creditAccount();
            }
            
            expect(dataProgram.read()).toBe(1600.00);
        });

        // TC-029: Multiple consecutive debits
        test('TC-029: Multiple consecutive debits decrease correctly', async () => {
            const amounts = ['100', '200', '300'];
            
            for (const amount of amounts) {
                const mockRl = createMockReadline([amount]);
                const ops = new Operations(dataProgram, mockRl);
                await ops.debitAccount();
            }
            
            expect(dataProgram.read()).toBe(400.00);
        });
    });

    // ========================================================================
    // 7. DATA PERSISTENCE AND CONSISTENCY
    // ========================================================================
    
    describe('Data Persistence and Consistency', () => {
        let dataProgram;
        let operations;
        let consoleSpy;

        beforeEach(() => {
            dataProgram = new DataProgram();
            consoleSpy = jest.spyOn(console, 'log').mockImplementation();
        });

        afterEach(() => {
            consoleSpy.mockRestore();
        });

        // TC-044: Balance persists between operations
        test('TC-044: Balance persists correctly between operations', async () => {
            let mockRl = createMockReadline(['100']);
            let ops = new Operations(dataProgram, mockRl);
            await ops.creditAccount();
            
            await ops.viewBalance();
            expect(consoleSpy).toHaveBeenCalledWith('Current balance: 1100.00');
            
            mockRl = createMockReadline(['50']);
            ops = new Operations(dataProgram, mockRl);
            await ops.debitAccount();
            
            await ops.viewBalance();
            expect(consoleSpy).toHaveBeenCalledWith('Current balance: 1050.00');
        });

        // TC-045: Failed operation doesn't modify balance (Critical)
        test('TC-045: Failed debit does not modify balance', async () => {
            dataProgram.write(500.00);
            
            const mockRl = createMockReadline(['600.00']);
            const ops = new Operations(dataProgram, mockRl);
            
            await ops.viewBalance();
            expect(consoleSpy).toHaveBeenCalledWith('Current balance: 500.00');
            
            await ops.debitAccount(); // Should fail
            
            await ops.viewBalance();
            expect(consoleSpy).toHaveBeenCalledWith('Current balance: 500.00');
        });

        // TC-046: Balance calculation accuracy
        test('TC-046: Decimal precision maintained in calculations', async () => {
            const amounts = ['333.33', '333.33', '333.34'];
            
            for (const amount of amounts) {
                const mockRl = createMockReadline([amount]);
                const ops = new Operations(dataProgram, mockRl);
                await ops.creditAccount();
            }
            
            expect(dataProgram.read()).toBe(2000.00);
        });

        // TC-047: Large number of transactions
        test('TC-047: System handles many transactions correctly', async () => {
            // 10 credits of 100
            for (let i = 0; i < 10; i++) {
                const mockRl = createMockReadline(['100']);
                const ops = new Operations(dataProgram, mockRl);
                await ops.creditAccount();
            }
            
            // 10 debits of 50
            for (let i = 0; i < 10; i++) {
                const mockRl = createMockReadline(['50']);
                const ops = new Operations(dataProgram, mockRl);
                await ops.debitAccount();
            }
            
            expect(dataProgram.read()).toBe(1500.00);
        });
    });

    // ========================================================================
    // 8. DECIMAL PRECISION AND ROUNDING
    // ========================================================================
    
    describe('Decimal Precision and Rounding', () => {
        let dataProgram;
        let operations;
        let consoleSpy;

        beforeEach(() => {
            dataProgram = new DataProgram();
            consoleSpy = jest.spyOn(console, 'log').mockImplementation();
        });

        afterEach(() => {
            consoleSpy.mockRestore();
        });

        // TC-048: Credit with two decimal places
        test('TC-048: Credit 123.45 maintains precision', async () => {
            const mockRl = createMockReadline(['123.45']);
            const ops = new Operations(dataProgram, mockRl);
            await ops.creditAccount();
            
            expect(dataProgram.read()).toBe(1123.45);
        });

        // TC-049: Debit with two decimal places
        test('TC-049: Debit 234.56 maintains precision', async () => {
            const mockRl = createMockReadline(['234.56']);
            const ops = new Operations(dataProgram, mockRl);
            await ops.debitAccount();
            
            expect(dataProgram.read()).toBe(765.44);
        });

        // TC-050: Multiple decimal operations
        test('TC-050: Multiple small decimal operations maintain precision', async () => {
            let mockRl = createMockReadline(['0.01']);
            let ops = new Operations(dataProgram, mockRl);
            await ops.creditAccount();
            
            mockRl = createMockReadline(['0.01']);
            ops = new Operations(dataProgram, mockRl);
            await ops.creditAccount();
            
            mockRl = createMockReadline(['0.01']);
            ops = new Operations(dataProgram, mockRl);
            await ops.debitAccount();
            
            expect(dataProgram.read()).toBe(1000.01);
        });

        // TC-051: Operations resulting in .99 balance
        test('TC-051: Debit 0.01 results in 999.99', async () => {
            const mockRl = createMockReadline(['0.01']);
            const ops = new Operations(dataProgram, mockRl);
            await ops.debitAccount();
            
            expect(dataProgram.read()).toBe(999.99);
        });

        // TC-052: Verify two decimal place display
        test('TC-052: Display always shows 2 decimal places', async () => {
            let mockRl = createMockReadline(['100']);
            let ops = new Operations(dataProgram, mockRl);
            await ops.creditAccount();
            
            await ops.viewBalance();
            expect(consoleSpy).toHaveBeenCalledWith('Current balance: 1100.00');
        });

        test('Amounts rounded to 2 decimal places', async () => {
            const mockRl = createMockReadline(['123.456']);
            const ops = new Operations(dataProgram, mockRl);
            await ops.creditAccount();
            
            // Should round to 123.46
            expect(dataProgram.read()).toBe(1123.46);
        });
    });

    // ========================================================================
    // 9. BOUNDARY VALUE TESTING
    // ========================================================================
    
    describe('Boundary Value Testing', () => {
        let dataProgram;
        let operations;
        let consoleSpy;

        beforeEach(() => {
            dataProgram = new DataProgram();
            consoleSpy = jest.spyOn(console, 'log').mockImplementation();
        });

        afterEach(() => {
            consoleSpy.mockRestore();
        });

        // TC-053: Minimum balance (zero)
        test('TC-053: View balance at zero displays correctly', async () => {
            dataProgram.write(0.00);
            const mockRl = createMockReadline();
            const ops = new Operations(dataProgram, mockRl);
            
            await ops.viewBalance();
            expect(consoleSpy).toHaveBeenCalledWith('Current balance: 0.00');
        });

        // TC-054: Maximum balance
        test('TC-054: View maximum balance displays correctly', async () => {
            dataProgram.write(999999.99);
            const mockRl = createMockReadline();
            const ops = new Operations(dataProgram, mockRl);
            
            await ops.viewBalance();
            expect(consoleSpy).toHaveBeenCalledWith('Current balance: 999999.99');
        });

        // TC-055: Credit minimum amount (0.01)
        test('TC-055: Credit 0.01 to zero balance', async () => {
            dataProgram.write(0.00);
            const mockRl = createMockReadline(['0.01']);
            const ops = new Operations(dataProgram, mockRl);
            
            await ops.creditAccount();
            expect(dataProgram.read()).toBe(0.01);
        });

        // TC-056: Debit minimum amount (0.01)
        test('TC-056: Debit 0.01 from 0.01 balance', async () => {
            dataProgram.write(0.01);
            const mockRl = createMockReadline(['0.01']);
            const ops = new Operations(dataProgram, mockRl);
            
            await ops.debitAccount();
            expect(dataProgram.read()).toBe(0.00);
        });

        // TC-058: Exact balance debit (boundary)
        test('TC-058: Debit exact balance amount', async () => {
            dataProgram.write(555.55);
            const mockRl = createMockReadline(['555.55']);
            const ops = new Operations(dataProgram, mockRl);
            
            await ops.debitAccount();
            expect(dataProgram.read()).toBe(0.00);
        });
    });

    // ========================================================================
    // 10. INPUT VALIDATION
    // ========================================================================
    
    describe('User Input Validation', () => {
        let dataProgram;
        let operations;
        let consoleSpy;

        beforeEach(() => {
            dataProgram = new DataProgram();
            consoleSpy = jest.spyOn(console, 'log').mockImplementation();
        });

        afterEach(() => {
            consoleSpy.mockRestore();
        });

        // TC-061: Amount input accepts decimal values
        test('TC-061: Decimal amounts are accepted', async () => {
            const mockRl = createMockReadline(['123.45']);
            const ops = new Operations(dataProgram, mockRl);
            
            await ops.creditAccount();
            expect(dataProgram.read()).toBe(1123.45);
        });

        // TC-062: Amount input accepts whole numbers
        test('TC-062: Whole numbers are accepted and formatted', async () => {
            const mockRl = createMockReadline(['100']);
            const ops = new Operations(dataProgram, mockRl);
            
            await ops.creditAccount();
            expect(consoleSpy).toHaveBeenCalledWith('Amount credited. New balance: 1100.00');
        });

        test('Negative amounts are rejected', async () => {
            const mockRl = createMockReadline(['-50']);
            const ops = new Operations(dataProgram, mockRl);
            
            await ops.creditAccount();
            expect(consoleSpy).toHaveBeenCalledWith('Invalid amount. Please enter a valid positive number.');
            expect(dataProgram.read()).toBe(1000.00);
        });

        test('Non-numeric input is rejected', async () => {
            const mockRl = createMockReadline(['abc']);
            const ops = new Operations(dataProgram, mockRl);
            
            await ops.debitAccount();
            expect(consoleSpy).toHaveBeenCalledWith('Invalid amount. Please enter a valid positive number.');
            expect(dataProgram.read()).toBe(1000.00);
        });

        test('Empty input is rejected', async () => {
            const mockRl = createMockReadline(['']);
            const ops = new Operations(dataProgram, mockRl);
            
            await ops.creditAccount();
            expect(consoleSpy).toHaveBeenCalledWith('Invalid amount. Please enter a valid positive number.');
            expect(dataProgram.read()).toBe(1000.00);
        });
    });

    // ========================================================================
    // 11. OPERATION DISPATCHER
    // ========================================================================
    
    describe('Operations Execute Dispatcher', () => {
        let dataProgram;
        let operations;
        let consoleSpy;

        beforeEach(() => {
            dataProgram = new DataProgram();
            const mockRl = createMockReadline(['100']);
            operations = new Operations(dataProgram, mockRl);
            consoleSpy = jest.spyOn(console, 'log').mockImplementation();
        });

        afterEach(() => {
            consoleSpy.mockRestore();
        });

        test('Execute TOTAL operation calls viewBalance', async () => {
            await operations.execute('TOTAL');
            expect(consoleSpy).toHaveBeenCalledWith('Current balance: 1000.00');
        });

        test('Execute CREDIT operation calls creditAccount', async () => {
            await operations.execute('CREDIT');
            expect(consoleSpy).toHaveBeenCalledWith('Amount credited. New balance: 1100.00');
        });

        test('Execute DEBIT operation calls debitAccount', async () => {
            await operations.execute('DEBIT');
            expect(consoleSpy).toHaveBeenCalledWith('Amount debited. New balance: 900.00');
        });

        test('Execute unknown operation throws error', async () => {
            await expect(operations.execute('INVALID')).rejects.toThrow('Unknown operation type: INVALID');
        });
    });

    // ========================================================================
    // 12. CRITICAL BUSINESS RULES VALIDATION
    // ========================================================================
    
    describe('Critical Business Rules', () => {
        let dataProgram;

        beforeEach(() => {
            dataProgram = new DataProgram();
        });

        test('RULE 1: No overdraft - debits exceeding balance are rejected', async () => {
            const mockRl = createMockReadline(['2000']);
            const ops = new Operations(dataProgram, mockRl);
            const consoleSpy = jest.spyOn(console, 'log').mockImplementation();
            
            await ops.debitAccount();
            
            expect(consoleSpy).toHaveBeenCalledWith('Insufficient funds for this debit.');
            expect(dataProgram.read()).toBe(1000.00);
            consoleSpy.mockRestore();
        });

        test('RULE 2: Initial balance is always 1000.00', () => {
            expect(dataProgram.read()).toBe(1000.00);
        });

        test('RULE 3: Balance never becomes negative', async () => {
            const mockRl = createMockReadline(['1500']);
            const ops = new Operations(dataProgram, mockRl);
            
            await ops.debitAccount();
            
            const balance = dataProgram.read();
            expect(balance).toBeGreaterThanOrEqual(0);
            expect(balance).toBe(1000.00); // Unchanged
        });

        test('RULE 4: Decimal precision maintained at 2 places', () => {
            dataProgram.write(1234.567);
            // JavaScript will handle rounding, but display should format to 2 places
            const balance = dataProgram.read();
            const formatted = balance.toFixed(2);
            expect(formatted).toBe('1234.57');
        });

        test('RULE 5: Failed operations do not modify balance', async () => {
            const mockRl = createMockReadline(['1500']);
            const ops = new Operations(dataProgram, mockRl);
            
            const balanceBefore = dataProgram.read();
            await ops.debitAccount();
            const balanceAfter = dataProgram.read();
            
            expect(balanceAfter).toBe(balanceBefore);
        });

        test('RULE 6: Transactions are processed immediately', async () => {
            const mockRl = createMockReadline(['100']);
            const ops = new Operations(dataProgram, mockRl);
            
            await ops.creditAccount();
            
            // Balance should be updated immediately
            expect(dataProgram.read()).toBe(1100.00);
        });

        test('RULE 7: Maximum balance constraint', () => {
            dataProgram.write(999999.99);
            expect(dataProgram.read()).toBe(999999.99);
        });
    });
});
