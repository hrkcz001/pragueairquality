const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');

module.exports = {
    // define entrypoints for JavaScript and SCSS
    entry: [
        './index.js',
    ],

    // define output
    output: {
        path: path.resolve(__dirname, 'dist'),
        filename: '[name].js',
        publicPath: '/static'
    },

    // define loaders for file types
    module: {
        rules: [
            // loading elm files
            {
                test: /\.elm$/,
                exclude: [/elm-stuff/, /node_modules/],
                // we want to use optimize=true when building for production
                use: {
                    loader: 'elm-webpack-loader',
                    options: {
                        verbose: true
                    }
                }
            }
        ]
    },

    // set noParse for elm files since we don't need webpack to resolve any imports there
    resolve: {
        extensions: ['.js', '.elm']
    },

    plugins: [
        // generate Html using HtmlWebpackPlugin
        new HtmlWebpackPlugin({
            title: 'Prague Air Quality'
        })
    ]
};
