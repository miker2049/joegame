module.exports = {
  "presets": [["@babel/preset-env", { "targets": "defaults", shippedProposals: true }],'@babel/preset-typescript'],
  "plugins": ["@babel/plugin-proposal-object-rest-spread","@babel/plugin-transform-runtime"],
  "sourceType": "unambiguous"
}
