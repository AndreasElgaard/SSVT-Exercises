/**
 * @type {import('../stryker-js/packages/api/core').StrykerOptions}
 */
module.exports = {
  packageManager: "npm",
  reporters: ["html", "clear-text", "progress", "json"],
  mutate: ["lib/numbers/basic.js"],
  testRunner: "mocha",
  coverageAnalysis: "perTest",
  concurrency: 4,
  timeoutMS: 2000,
  mutator: {
    excludedMutations: ['BlockStatement']
  },
  mochaOptions: {
    ui: "tdd",
  },
  plugins: [require.resolve('../stryker-js/packages/mocha-runner')],
};
