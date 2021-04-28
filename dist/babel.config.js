"use strict";
module.exports = {
    sourceType: "unambiguous",
    plugins: [
        "@babel/plugin-transform-typescript",
        "@babel/plugin-proposal-object-rest-spread",
        "@babel/plugin-transform-runtime"
    ],
    presets: [
        [
            "@babel/preset-env",
            {
                shippedProposals: true,
                targets: "defaults"
            }
        ],
        // "@babel/preset-typescript"
    ]
};
//# sourceMappingURL=babel.config.js.map