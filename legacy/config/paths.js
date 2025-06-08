const path = require('path');

module.exports = {
  entry: path.resolve('./src/index'),
  componententry: path.resolve('./src/index.component.js'),
  dist: path.resolve('./dist'),
  template: path.resolve('./src/index.html'),
  favicon: path.resolve('./src/assets/favicon.ico'),
  ownModules: path.resolve(__dirname, '../node_modules'),
  scripts: path.resolve(__dirname, '../scripts'),
  elmMake: path.resolve(__dirname, '../node_modules/.bin/elm-make')
};
