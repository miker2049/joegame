import cors from 'cors';
import express, { Express } from 'express';
// import path from 'path'
import http from 'http';
import JdbController from '../src/buildtools/JdbController';
import { JdbAsset, JdbBody, JdbCharacter, JdbImage, JdbMapObject, JdbTable, JdbTableNames } from '../src/buildtools/JdbModel';

// import path from 'path'
const jdb = new JdbController('assets/jdb.db');
const app: Express = express();

app.use(cors())
app.use(express.json({limit: '250mb'}));
app.use(express.urlencoded({extended: false, limit: '250mb'}));
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

const server = http.createServer(app);

/**
 * Listen on provided port, on all network interfaces.
 */
server.listen(port);
server.on('error', onError);
server.on('listening', onListening);

function validateTableName(name: string): JdbTableNames | false {
        switch(name){
            case JdbTableNames.assets:
                return JdbTableNames.assets
            case JdbTableNames.images:
                return JdbTableNames.images
            case JdbTableNames.mapobjects:
                return JdbTableNames.mapobjects
            case JdbTableNames.bodies:
                return JdbTableNames.bodies
            case JdbTableNames.characters:
                return JdbTableNames.characters
            default: return false
        }
}

app.get('/api/nids/:table',async (req,res, next)=>{
    const max: number = Number(req.query.amount) || 10
    const min: number = Number(req.query.offset) || 0
    try {
        const isTable = validateTableName(req.params.table)
        if(isTable){
            res.json(await jdb.getIds(isTable,max,min));
        } else {
            res.json({msg: `Table '${req.params.table}' is not around.`})
        }
    } catch (err) {
        next(err)
    }
})

app.get('/api/entity/:table/:id',async (req,res, next)=>{
    const id = Number(req.params.id)
    if(isNaN(id)) res.json({msg: "ID malformed"})
    try {

        const isTable = validateTableName(req.params.table)
        if(isTable){
            res.json(await jdb.selectById(isTable,id));
        } else {
            res.json({msg: `Table '${req.params.table}' is not around.`})
        }
    } catch (err) {
        next(err)
    }
})
app.post('/api/entity/:table', async (req,res, next)=>{
    try{
        const isTable = validateTableName(req.params.table)
        if(isTable){
            res.json(await jdb.insertRow(isTable,req.body));
        } else {
            res.json({msg: `Table '${req.params.table}' is not around.`})
        }
    } catch (err) {
        next(err)
    }
})

app.patch('/api/entity/:table/:id', async (req,res, next)=>{
    const id = Number(req.params.id)
    if(isNaN(id)) res.json({msg: "ID malformed"})
    const body = {id, ...req.body}
    try {
        const isTable = validateTableName(req.params.table)
        if(isTable){
            res.json(await jdb.updateRow(isTable,body));
        } else {
            res.json({msg: `Table '${req.params.table}' is not around.`})
        }
    } catch (err) {
        next(err)
    }
})

/**
 * Normalize a port into a number, string, or false.
 */

function normalizePort(val: string) {
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

function onError(error: Error) {
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
