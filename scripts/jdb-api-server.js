"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const tslib_1 = require("tslib");
const express_1 = (0, tslib_1.__importDefault)(require("express"));
const JdbController_1 = (0, tslib_1.__importDefault)(require("../src/buildtools/JdbController"));
// import path from 'path'
const jdb = new JdbController_1.default('assets/jdb.db');
// import path from 'path'
const http_1 = (0, tslib_1.__importDefault)(require("http"));
const app = (0, express_1.default)();
app.use(express_1.default.json());
app.use(express_1.default.urlencoded({ extended: false }));
// app.use(express.static(path.join(__dirname, 'public')));
// app.use('/', indexRouter);
// app.use('/users', usersRouter);
/**
 * Get port from environment and store in Express.
 */
const port = normalizePort(process.env.PORT || '3000');
app.set('port', port);
/**
 * Create HTTP server.
 */
const server = http_1.default.createServer(app);
/**
 * Listen on provided port, on all network interfaces.
 */
server.listen(port);
server.on('error', onError);
server.on('listening', onListening);
app.get('/assets/:id', (req, res) => (0, tslib_1.__awaiter)(void 0, void 0, void 0, function* () {
    const found = yield jdb.getAssetById(Number(req.params.id));
    res.json(found);
}));
app.post('/assets/create', (req, res) => (0, tslib_1.__awaiter)(void 0, void 0, void 0, function* () {
    const id = yield jdb.createAsset(req.body);
    res.json(id);
}));
app.patch('/assets/:id', (req, res) => (0, tslib_1.__awaiter)(void 0, void 0, void 0, function* () {
    console.log("poop");
    const id = yield jdb.updateAsset(req.body, Number(req.params.id));
    res.json(id);
}));
const cb0 = function (_req, _res, next) {
    console.log('CB0');
    next();
};
const cb1 = function (_req, _res, next) {
    console.log('CB1');
    next();
};
app.get('/example/d', [cb0, cb1], (_req, _res, next) => {
    console.log('the response will be sent by the next function ...');
    next();
}, (_req, res, next) => {
    res.send('Hello from D!');
    next();
}, [cb0, cb0]);
/**
 * Normalize a port into a number, string, or false.
 */
function normalizePort(val) {
    const port = parseInt(val, 10);
    if (isNaN(port)) {
        // named pipe
        return val;
    }
    if (port >= 0) {
        // port number
        return port;
    }
    return false;
}
/**
 * Event listener for HTTP server "error" event.
 */
function onError(error) {
    if (error.name !== 'listen') {
        throw error;
    }
    const bind = typeof port === 'string'
        ? 'Pipe ' + port
        : 'Port ' + port;
    // handle specific listen errors with friendly messages
    switch (error.message) {
        case 'EACCES':
            console.error(bind + ' requires elevated privileges');
            process.exit(1);
            break;
        case 'EADDRINUSE':
            console.error(bind + ' is already in use');
            process.exit(1);
            break;
        default:
            throw error;
    }
}
/**
 * Event listener for HTTP server "listening" event.
 */
function onListening() {
    // const addr = server.address();
    // const bind = typeof addr === 'string'
    //   ? 'pipe ' + addr
    //   : 'port ' + addr.port;
}
