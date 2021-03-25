const path = require('path')
const { merge } = require('webpack-merge')
const common = require('./webpack.common')
const JavaScriptObfuscator = require('webpack-obfuscator')
const { CleanWebpackPlugin } = require('clean-webpack-plugin')
const { DefinePlugin } = require('webpack')
const { InjectManifest, GenerateSW } = require('workbox-webpack-plugin')
const CompressionPlugin = require('compression-webpack-plugin')

const prod = {
  mode: 'production',
  // devtool: 'source-map',
  watch: false,
  output: {
    path: path.resolve(__dirname, '../dist'),
    filename: 'joegame-lib.min.js',
    library: 'joegame-lib',
    libraryTarget: 'umd',
  },
  optimization: {
    minimize: true
  },
  plugins: [
    // new CleanWebpackPlugin({ cleanOnceBeforeBuildPatterns: [path.resolve(__dirname, '../public/*.js')] }),
    new DefinePlugin({
      "ASSETPATH": '"/"',
      "DEVELOPMENT": JSON.stringify(false)
    }),
  ]
}

module.exports = merge(common, prod)
