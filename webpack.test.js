const path = require('path')
const html = require('html-webpack-plugin')
const CopyPlugin = require('copy-webpack-plugin')
var glob = require("glob");
const babelOptions = require('./babel.config')

module.exports = {
  mode: 'development',
  entry: {
    tests: glob.sync('./test/*.test.ts'),
    soundDebug: './test/soundDebug.ts'
  },
  devServer: {
    contentBase: path.join(__dirname, 'testdist'),
  },
  target: 'web',
  output: {
    path: path.resolve(__dirname, 'testdist'),
    filename: '[name].[fullhash].bundle.js'
  },
  resolve: {
    extensions: ['.ts', '.js'],
    fallback: {
      fs: false,
      path: false,
    }
  },
  module: {
    rules: [
      {
        test: /\.ts$/,
        include: [
          path.resolve(__dirname, "src"),
        ],
        use: [
          {
            loader: 'babel-loader',
            options: babelOptions
          },
          // 'source-map-loader'
        ]
      },
      {
        test: /\.js$/,
        include: [
          path.resolve(__dirname, "../src"),
        ],
        use: [
          {
            loader: 'babel-loader',
            options: babelOptions
          },
          // 'source-map-loader'
        ]
      },
      {
        test: /\.csv$/i,
        exclude: /node_modules/,
        use: 'raw-loader'
      }
    ]
  },
  optimization: {
  },
  plugins: [
    new html({
      template: './test/index.ejs',
      chunks: ['tests'],
      inject: false
    }),
    new html({
      filename: 'sounddebug.html',
      chunks: ['soundDebug']
    }),
    new CopyPlugin({
      patterns: [
        { from: "./test/assets", to: "assets" },
        { from: "./node_modules/mocha/mocha.js", to: "" },
        { from: "./node_modules/mocha/mocha.css", to: "" },
      ],
    }),
  ]
}
