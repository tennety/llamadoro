const autoprefixer = require("autoprefixer");
const purgecss = require("@fullhuman/postcss-purgecss");

const development = {
  plugins: [autoprefixer]
};

const production = {
  plugins: [
    purgecss({
      content: ["./src/**/*.elm", "index.js"],
      whitelist: ["html", "body"]
    }),
    autoprefixer
  ]
};

if (process.env.NODE_ENV === "production") {
  module.exports = production;
} else {
  module.exports = development;
}
