const path = require('path')
const html = require('html-webpack-plugin')

const babelOptions = require('./babel.config')

module.exports = {
  mode: 'development',
  entry: {
    main: './test/main.test.ts',
  },
  devServer: {
    contentBase: path.join(__dirname, 'testdist'),
    hot: true
  },
  target: 'web',
  output: {
    path: path.resolve(__dirname, 'testdist'),
    filename: 'test.[hash].bundle.js'
  },
  resolve: {
    extensions: ['.ts', '.js'],
    fallback: {
      fs: false,
      path: false,
      stream: require.resolve("stream-browserify")
    },
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
    })
  ]
}
