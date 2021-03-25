const { merge } = require('webpack-merge')
const { DefinePlugin } = require('webpack')
const common = require('./webpack.common')
const path = require('path')
// const BundleAnalyzerPlugin = require('webpack-bundle-analyzer').BundleAnalyzerPlugin;

const dev = {
  mode: 'development',
  devtool: 'source-map',
  entry: {
    test: './test/joegame.test.ts'
  },
  output: {
    path: path.resolve(__dirname, '../public/joegame')
  },
  // devServer: {
  //   open: true,
  //   publicPath: '/joegame',
  //   contentBase: path.join(__dirname, 'public'),
  //   port: 8081
  // },
  plugins: [
    // new BundleAnalyzerPlugin()
    new DefinePlugin({
      "ASSETPATH": '"/joegame/"',
      "DEVELOPMENT": JSON.stringify(true)
    })
  ]
}

module.exports = merge(common, dev)
