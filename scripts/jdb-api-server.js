"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const tslib_1 = require("tslib");
const JdbModel_1 = require("../src/buildtools/JdbModel");
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
app.get('/:table/:id', (req, res) => (0, tslib_1.__awaiter)(void 0, void 0, void 0, function* () {
    const id = Number(req.params.id);
    if (isNaN(id))
        res.json({ msg: "ID malformed" });
    switch (req.params.table) {
        case JdbModel_1.JdbTableNames.assets:
            res.json(yield jdb.selectById(JdbModel_1.JdbTableNames.assets, id));
            break;
        case JdbModel_1.JdbTableNames.images:
            res.json(yield jdb.selectById(JdbModel_1.JdbTableNames.images, id));
            break;
        case JdbModel_1.JdbTableNames.mapobjects:
            res.json(yield jdb.selectById(JdbModel_1.JdbTableNames.mapobjects, id));
            break;
        case JdbModel_1.JdbTableNames.bodies:
            res.json(yield jdb.selectById(JdbModel_1.JdbTableNames.bodies, id));
            break;
        case JdbModel_1.JdbTableNames.characters:
            res.json(yield jdb.selectById(JdbModel_1.JdbTableNames.characters, id));
            break;
        default: res.json({ msg: "Table note found" });
    }
}));
app.post('/:table/create', (req, res) => (0, tslib_1.__awaiter)(void 0, void 0, void 0, function* () {
    switch (req.params.table) {
        case JdbModel_1.JdbTableNames.assets:
            res.json(yield jdb.insertRow(JdbModel_1.JdbTableNames.assets, req.body));
            break;
        case JdbModel_1.JdbTableNames.images:
            res.json(yield jdb.insertRow(JdbModel_1.JdbTableNames.images, req.body));
            break;
        case JdbModel_1.JdbTableNames.mapobjects:
            res.json(yield jdb.insertRow(JdbModel_1.JdbTableNames.mapobjects, req.body));
            break;
        case JdbModel_1.JdbTableNames.bodies:
            res.json(yield jdb.insertRow(JdbModel_1.JdbTableNames.bodies, req.body));
            break;
        case JdbModel_1.JdbTableNames.characters:
            res.json(yield jdb.insertRow(JdbModel_1.JdbTableNames.characters, req.body));
            break;
        default: res.json({ msg: "Table note found" });
    }
}));
app.patch('/:table/:id', (req, res) => (0, tslib_1.__awaiter)(void 0, void 0, void 0, function* () {
    const id = Number(req.params.id);
    if (isNaN(id))
        res.json({ msg: "ID malformed" });
    const body = Object.assign({ id }, req.body);
    switch (req.params.table) {
        case JdbModel_1.JdbTableNames.assets:
            res.json(yield jdb.updateRow(JdbModel_1.JdbTableNames.assets, body));
            break;
        case JdbModel_1.JdbTableNames.images:
            res.json(yield jdb.updateRow(JdbModel_1.JdbTableNames.images, body));
            break;
        case JdbModel_1.JdbTableNames.mapobjects:
            res.json(yield jdb.updateRow(JdbModel_1.JdbTableNames.mapobjects, body));
            break;
        case JdbModel_1.JdbTableNames.bodies:
            res.json(yield jdb.updateRow(JdbModel_1.JdbTableNames.bodies, body));
            break;
        case JdbModel_1.JdbTableNames.characters:
            res.json(yield jdb.updateRow(JdbModel_1.JdbTableNames.characters, body));
            break;
        default: res.json({ msg: "Table note found" });
    }
}));
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
