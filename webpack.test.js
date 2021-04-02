const path = require('path')
const html = require('html-webpack-plugin')
const CopyPlugin = require('copy-webpack-plugin')

const babelOptions = require('./babel.config')

module.exports = {
  mode: 'development',
  entry: {
    main: './test/main.test.ts',
  },
  devServer: {
    contentBase: path.join(__dirname, 'testdist'),
    watchContentBase: true,
    disableHostCheck: true,
    hot: true,
    // sockHost: 'https://code.groupchattt.page',
    // sockPath: '/proxy/8080/sockjs-node',
    // https: true
    // proxy: {
    // proxy: {
    //   '/sockjs-node': 
    // },
  },
  target: 'web',
  output: {
    path: path.resolve(__dirname, 'testdist'),
    filename: 'test.[fullhash].bundle.js'
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
          'source-map-loader'
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
          'source-map-loader'
        ]
      }
    ]
  },
  optimization: {
  },
  plugins: [
    new html({
      template: './test/index.ejs',
      inject: false
    }),
    new CopyPlugin({
      patterns: [
        { from: "./node_modules/mocha/mocha.js", to: "" },
        { from: "./node_modules/mocha/mocha.css", to: "" },
      ],
    }), 
  ]
}
