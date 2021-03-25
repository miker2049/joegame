const { WebpackManifestPlugin } = require('webpack-manifest-plugin')
const BundleAnalyzerPlugin = require('webpack-bundle-analyzer').BundleAnalyzerPlugin;
const path = require('path')
const CopyWebpackPlugin = require('copy-webpack-plugin')
const { InjectManifest, GenerateSW } = require('workbox-webpack-plugin')

const babelOptions = require('../babel.config')

module.exports = {
  entry: {
    main: ['./src/index.ts', './webpack/credits.js'],
  },
  output: {
    path: path.resolve(__dirname, '../dist/'),
    publicPath: './',
    filename: '[name].bundle.js',
    chunkFilename: '[name].chunk.js',
  },
  resolve: {
    extensions: ['.ts', '.js'],
    fallback: {
      fs: false
    },
  },
  module: {
    rules: [
      // https://github.com/webpack/webpack/issues/11543#issuecomment-745529334
      {
        test: /\.ts$/,
        exclude: /node_modules/,
        use: [
          // {
          //   loader: 'babel-loader',
          //   options: babelOptions
          // },
          {
            loader: 'ts-loader',
            options: {
              transpileOnly: true,
              configFile: path.resolve(__dirname, '../tsconfig.json'),
            }
          },
          'source-map-loader'
        ]
      },
      {
        test: /\.js$/,
        exclude: /node_modules/,
        use: [
          {
            loader: 'babel-loader',
            options: babelOptions
          },
          'source-map-loader'
        ]
      },
      {
        test: /\.frag$/i,
        exclude: /node_modules/,
        use: 'raw-loader'
      },
      {
        test: /\.glsl$/i,
        exclude: /node_modules/,
        use: 'raw-loader'
      }
    ]
  },
  optimization: {
    splitChunks: {
      cacheGroups: {
        commons: {
          test: /[\\/]node_modules[\\/]/,
          name: 'vendors',
          chunks: 'all',
          filename: '[name].bundle.js'
        }
      }
    }
  },
  plugins: [
  ]
}
