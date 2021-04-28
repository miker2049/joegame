"use strict";
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
const chokidar = require('chokidar');
const liveServer = require('live-server');
const esbuild = require('esbuild');
(() => __awaiter(void 0, void 0, void 0, function* () {
    // Initialize watcher.
    const watcher = chokidar.watch(['./test/assets/*', './test/*.test.js', './src/**/*'], {
        // ignored: /(^|[\/\\])\../, // ignore dotfiles
        persistent: true
    });
    const result = yield esbuild.build({
        entryPoints: ['test/index.test.ts'],
        target: [
            'chrome78',
            'firefox67',
            'safari13',
            'edge66',
        ],
        bundle: true,
        // watch: true,
        incremental: true,
        outfile: 'test/test-bundle.js',
        sourcemap: 'inline',
        loader: {
            '.png': 'dataurl',
            '.csv': 'text',
        },
    }).catch(err => console.error(err));
    console.log(result);
    console.log(`server running at localhost:8001`);
    // Call "stop" on the web server when you're done
    // server.stop()
    //
    // Add event listeners.
    watcher.on('change', (path, stats) => {
        console.log(`File ${path, stats} has been changed`);
        result.rebuild().then(msg => console.log(msg), err => console.error(err));
    });
    liveServer.start({
        root: 'test',
        port: 8001,
        open: false,
    });
}))();
//# sourceMappingURL=esbuild.test.js.map