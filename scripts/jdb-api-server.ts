import { JdbAsset, JdbBody, JdbCharacter, JdbImage, JdbMapObject, JdbTableNames } from '../src/buildtools/JdbModel'
import express, { Express, NextFunction, RequestHandler, Request, Response } from 'express'
import JdbController from '../src/buildtools/JdbController'

// import path from 'path'
const jdb = new JdbController('assets/jdb.db');
// import path from 'path'
import http from 'http'
const app: Express = express();

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
app.get('/:table/:id',async (req,res, next)=>{
    const id = Number(req.params.id)
    if(isNaN(id)) res.json({msg: "ID malformed"})
    try {
        switch(req.params.table){
            case JdbTableNames.assets:
                res.json(await jdb.selectById<JdbAsset>(JdbTableNames.assets,id));
                break;
            case JdbTableNames.images:
                res.json(await jdb.selectById<JdbImage>(JdbTableNames.images,id))
                break;
            case JdbTableNames.mapobjects:
                res.json(await jdb.selectById<JdbMapObject>(JdbTableNames.mapobjects,id))
                break;
            case JdbTableNames.bodies:
                res.json(await jdb.selectById<JdbBody>(JdbTableNames.bodies,id))
                break;
            case JdbTableNames.characters:
                res.json(await jdb.selectById<JdbCharacter>(JdbTableNames.characters,id))
                break;
            default: res.json({msg: "Table note found"})
        }

    } catch (err) {
        next(err)
    }
})
app.post('/:table', async (req,res, next)=>{
    try{
        switch(req.params.table){
            case JdbTableNames.assets:
                res.json(await jdb.insertRow<JdbAsset>(JdbTableNames.assets,req.body));
                break;
            case JdbTableNames.images:
                res.json(await jdb.insertRow<JdbImage>(JdbTableNames.images,req.body))
                break;
            case JdbTableNames.mapobjects:
                res.json(await jdb.insertRow<JdbMapObject>(JdbTableNames.mapobjects,req.body))
                break;
            case JdbTableNames.bodies:
                res.json(await jdb.insertRow<JdbBody>(JdbTableNames.bodies,req.body))
                break;
            case JdbTableNames.characters:
                res.json(await jdb.insertRow<JdbCharacter>(JdbTableNames.characters,req.body))
                break;
            default: res.json({msg: "Table note found"})
        }
    } catch (err) {
        next(err)
    }
})

app.patch('/:table/:id', async (req,res, next)=>{
    const id = Number(req.params.id)
    if(isNaN(id)) res.json({msg: "ID malformed"})
    const body = {id, ...req.body}
    try {
        switch(req.params.table){
            case JdbTableNames.assets:
                res.json(await jdb.updateRow<JdbAsset>(JdbTableNames.assets,body));
                break;
            case JdbTableNames.images:
                res.json(await jdb.updateRow<JdbImage>(JdbTableNames.images,body))
                break;
            case JdbTableNames.mapobjects:
                res.json(await jdb.updateRow<JdbMapObject>(JdbTableNames.mapobjects,body))
                break;
            case JdbTableNames.bodies:
                res.json(await jdb.updateRow<JdbBody>(JdbTableNames.bodies,body))
                break;
            case JdbTableNames.characters:
                res.json(await jdb.updateRow<JdbCharacter>(JdbTableNames.characters,body))
                break;
            default: res.json({msg: "Table note found"})
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
