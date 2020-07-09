const webpack = require("webpack");
const path = require("path");
const MiniCssExtractPlugin = require("mini-css-extract-plugin");
const CompressionPlugin = require("compression-webpack-plugin");

module.exports = {
  entry: ["./src/main.ts", "./src/scss/main.scss"],
  devtool: "inline-source-map",
  plugins: [
    new CompressionPlugin(),
    new MiniCssExtractPlugin({
      filename: "style.css",
      chunkFilename: "styleid.css",
    }),
    new webpack.ProvidePlugin({
      $: "jquery",
      jQuery: "jquery",
      "window.jQuery": "jquery",
      "window.$": "jquery",
    }),
  ],
  module: {
    rules: [
      {
        test: /\.tsx?$/,
        use: "ts-loader",
        exclude: /node_modules/,
      },
      {
        test: /\.s[ac]ss$/i,
        use: [MiniCssExtractPlugin.loader, "css-loader", "sass-loader"],
        exclude: /node_modules/,
      },
      {
        test: /datatables\.net.*/,
        use: [
          {
            loader: "imports-loader",
            options: {
              imports: [
                {
                  // syntax: "default",
                  moduleName: "datatables.net",
                  name: "Datatables",
                },
              ],
            },
          },
        ],
      },
      {
        test: /\.(png|svg|jpg|gif)$/,
        use: ["file-loader"],
      },
    ],
  },
  resolve: {
    extensions: [".tsx", ".ts", ".js"],
  },
  output: {
    filename: "bundle.js",
    path: path.resolve(__dirname),
  },
};
