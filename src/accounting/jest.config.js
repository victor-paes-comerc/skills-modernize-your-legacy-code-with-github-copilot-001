module.exports = {
  testEnvironment: 'node',
  coverageDirectory: 'coverage',
  collectCoverageFrom: [
    'index.js',
    '!index.test.js'
  ],
  testMatch: [
    '**/*.test.js'
  ],
  coverageThreshold: {
    global: {
      statements: 50,
      branches: 75,
      functions: 60,
      lines: 50
    }
  }
};
