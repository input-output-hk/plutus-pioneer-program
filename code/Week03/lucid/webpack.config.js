const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const CopyWebpackPlugin = require('copy-webpack-plugin');
const NodePolyfillPlugin = require('node-polyfill-webpack-plugin');
const { ProvidePlugin } = require('webpack');
const dotenv = require('dotenv-webpack');

module.exports = {
    experiments: {
        asyncWebAssembly: true,
        topLevelAwait: true,
    },
    mode: 'development',
    entry: './src/index.js',
    devtool: 'inline-source-map',
    devServer: {
        static: path.join(__dirname, 'dist'),
        port: 9081,
        hot: true
    },
    plugins: [
        new HtmlWebpackPlugin({
            title: 'Vesting',
            template: './src/index.html'
        }),
        new NodePolyfillPlugin(),
        new ProvidePlugin({
            process: 'process/browser.js'
        }),
        new dotenv(),
    ],
    output: {
        filename: 'index.js',
        path: path.resolve(__dirname, 'dist'),
        clean: true,
    },
    module: {
        rules: [
            {
                test: /\.css$/,
                use: [
                    { loader: 'style-loader' },
                    { loader: 'css-loader', options: { importLoaders: 1 } },
                    {
                        loader: 'postcss-loader', options: {
                            postcssOptions: {
                                plugins: () => [
                                    require('autoprefixer')
                                ]
                            }
                        }
                    },
                    { loader: 'sass-loader' }
                ]
            }
        ],
    },
    resolve: {
        alias: {
            'jquery': path.join(__dirname, 'node_modules/jquery/src/jquery')
        },
        extensions: ['.tsx', '.ts', '.js', '.jsx'],
    }
};

