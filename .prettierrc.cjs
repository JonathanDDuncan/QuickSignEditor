module.exports = {
  plugins: [require('prettier-plugin-svelte')],
  semi: true,
  singleQuote: true,
  printWidth: 100,
  tabWidth: 2,
  trailingComma: 'none',
  svelteSortOrder: 'scripts-markup-options-styles',
  svelteStrictMode: true,
  svelteBracketNewLine: true,
  svelteAllowShorthand: false,
  svelteIndentScriptAndStyle: true
};
