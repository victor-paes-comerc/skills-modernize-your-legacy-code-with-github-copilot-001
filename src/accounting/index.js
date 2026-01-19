#!/usr/bin/env node

/**
 * Account Management System - Node.js Implementation
 * Converted from COBOL legacy application
 * 
 * This application preserves the original business logic, data integrity,
 * and menu options from the COBOL implementation.
 * 
 * Architecture (following COBOL pattern):
 * - MainProgram: User interface and program orchestration
 * - Operations: Business logic and transaction processing
 * - DataProgram: Data persistence layer
 */

const readline = require('readline');

/**
 * DataProgram - Data persistence layer
 * Equivalent to data.cob in COBOL application
 * 
 * Manages in-memory storage for account balance
 * Provides READ and WRITE operations
 */
class DataProgram {
    constructor() {
        // Initialize with default balance (same as COBOL: 1000.00)
        this.storageBalance = 1000.00;
    }

    /**
     * READ operation - Retrieves current balance
     * @returns {number} Current balance
     */
    read() {
        return this.storageBalance;
    }

    /**
     * WRITE operation - Updates stored balance
     * @param {number} balance - New balance value
     */
    write(balance) {
        this.storageBalance = balance;
    }

    /**
     * Main interface matching COBOL's CALL pattern
     * @param {string} operation - 'READ' or 'WRITE'
     * @param {number} balance - Balance value (for WRITE operations)
     * @returns {number} Balance (for READ operations)
     */
    execute(operation, balance = null) {
        if (operation === 'READ') {
            return this.read();
        } else if (operation === 'WRITE') {
            this.write(balance);
            return balance;
        }
        throw new Error(`Unknown operation: ${operation}`);
    }
}

/**
 * Operations - Business logic layer
 * Equivalent to operations.cob in COBOL application
 * 
 * Implements core business logic for all account operations
 */
class Operations {
    constructor(dataProgram, rl) {
        this.dataProgram = dataProgram;
        this.rl = rl;
    }

    /**
     * View Balance Operation (TOTAL)
     * Retrieves and displays current balance
     * @returns {Promise<void>}
     */
    async viewBalance() {
        const balance = this.dataProgram.execute('READ');
        console.log(`Current balance: ${balance.toFixed(2)}`);
    }

    /**
     * Credit Account Operation (CREDIT)
     * Prompts for amount, adds to balance, updates storage
     * @returns {Promise<void>}
     */
    async creditAccount() {
        const amount = await this.promptForAmount('Enter credit amount: ');
        
        if (amount === null) {
            return; // User cancelled
        }

        const currentBalance = this.dataProgram.execute('READ');
        const newBalance = Math.round((currentBalance + amount) * 100) / 100;
        
        this.dataProgram.execute('WRITE', newBalance);
        console.log(`Amount credited. New balance: ${newBalance.toFixed(2)}`);
    }

    /**
     * Debit Account Operation (DEBIT)
     * Prompts for amount, validates sufficient funds, processes debit
     * @returns {Promise<void>}
     */
    async debitAccount() {
        const amount = await this.promptForAmount('Enter debit amount: ');
        
        if (amount === null) {
            return; // User cancelled
        }

        const currentBalance = this.dataProgram.execute('READ');
        
        // Business Rule: Check for sufficient funds (no overdraft allowed)
        if (currentBalance >= amount) {
            const newBalance = Math.round((currentBalance - amount) * 100) / 100;
            this.dataProgram.execute('WRITE', newBalance);
            console.log(`Amount debited. New balance: ${newBalance.toFixed(2)}`);
        } else {
            console.log('Insufficient funds for this debit.');
        }
    }

    /**
     * Prompt user for transaction amount
     * @param {string} prompt - Prompt message
     * @returns {Promise<number|null>} Amount entered or null if invalid
     */
    promptForAmount(prompt) {
        return new Promise((resolve) => {
            this.rl.question(prompt, (input) => {
                const amount = parseFloat(input);
                
                if (isNaN(amount) || amount < 0) {
                    console.log('Invalid amount. Please enter a valid positive number.');
                    resolve(null);
                } else {
                    // Round to 2 decimal places (matching COBOL PIC 9(6)V99)
                    resolve(Math.round(amount * 100) / 100);
                }
            });
        });
    }

    /**
     * Main operation dispatcher
     * Equivalent to COBOL's EVALUATE statement
     * @param {string} operationType - Operation type ('TOTAL', 'CREDIT', 'DEBIT')
     * @returns {Promise<void>}
     */
    async execute(operationType) {
        if (operationType === 'TOTAL') {
            await this.viewBalance();
        } else if (operationType === 'CREDIT') {
            await this.creditAccount();
        } else if (operationType === 'DEBIT') {
            await this.debitAccount();
        } else {
            throw new Error(`Unknown operation type: ${operationType}`);
        }
    }
}

/**
 * MainProgram - User interface and orchestration
 * Equivalent to main.cob in COBOL application
 * 
 * Displays menu, accepts user input, routes to operations
 */
class MainProgram {
    constructor() {
        this.rl = readline.createInterface({
            input: process.stdin,
            output: process.stdout
        });
        
        this.dataProgram = new DataProgram();
        this.operations = new Operations(this.dataProgram, this.rl);
        this.continueFlag = true;
    }

    /**
     * Display the main menu
     */
    displayMenu() {
        console.log('--------------------------------');
        console.log('Account Management System');
        console.log('1. View Balance');
        console.log('2. Credit Account');
        console.log('3. Debit Account');
        console.log('4. Exit');
        console.log('--------------------------------');
    }

    /**
     * Prompt for user menu choice
     * @returns {Promise<number>} User's choice (1-4)
     */
    promptForChoice() {
        return new Promise((resolve) => {
            this.rl.question('Enter your choice (1-4): ', (input) => {
                const choice = parseInt(input);
                resolve(choice);
            });
        });
    }

    /**
     * Process user's menu choice
     * Equivalent to COBOL's EVALUATE USER-CHOICE
     * @param {number} choice - User's menu selection
     * @returns {Promise<void>}
     */
    async processChoice(choice) {
        switch (choice) {
            case 1:
                await this.operations.execute('TOTAL');
                break;
            case 2:
                await this.operations.execute('CREDIT');
                break;
            case 3:
                await this.operations.execute('DEBIT');
                break;
            case 4:
                this.continueFlag = false;
                break;
            default:
                console.log('Invalid choice, please select 1-4.');
        }
    }

    /**
     * Main program loop
     * Equivalent to COBOL's PERFORM UNTIL CONTINUE-FLAG = 'NO'
     */
    async run() {
        while (this.continueFlag) {
            this.displayMenu();
            const choice = await this.promptForChoice();
            await this.processChoice(choice);
        }

        console.log('Exiting the program. Goodbye!');
        this.rl.close();
    }
}

/**
 * Application entry point
 * Starts the MainProgram
 */
async function main() {
    try {
        const app = new MainProgram();
        await app.run();
        process.exit(0);
    } catch (error) {
        console.error('An error occurred:', error.message);
        process.exit(1);
    }
}

// Start the application
if (require.main === module) {
    main();
}

// Export for testing
module.exports = { MainProgram, Operations, DataProgram };
