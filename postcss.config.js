module.exports = {
  plugins: [
    // Sass-like imports
    require('postcss-import')({from: 'src/css/main.css'}),

    require('autoprefixer')({browsers: 'last 2 versions'}),

    // Error logging
    require('postcss-reporter')(),
  ]
};
