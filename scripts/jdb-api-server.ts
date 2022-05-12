import { Asset } from '../src/types/jdb-types'
import express, { Express, NextFunction, RequestHandler, Request, Response } from 'express'
import JdbController from '../src/buildtools/JdbController'

// import path from 'path'
const jdb = new JdbController('assets/jdb.db');
// import path from 'path'
import http from 'http'
const app: Express = express();

app.use(express.json());
app.use(express.urlencoded({ extended: false }));
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
app.get('/assets/:id',async (req,res)=>{
    const found = await jdb.getAssetById(Number(req.params.id))
    res.json(found)
})
app.post('/assets/create', async (req,res)=>{
    const id = await jdb.createAsset(req.body as Asset)
    res.json(id)
})

app.patch('/assets/:id', async (req,res)=>{
    console.log("poop")
    const id = await jdb.updateAsset(req.body as Asset, Number(req.params.id))
    res.json(id)
})


const cb0: RequestHandler = function(_req, _res, next) {
    console.log('CB0')
    next()
}

const cb1: RequestHandler = function(_req, _res, next) {
    console.log('CB1')
    next()
}

app.get('/example/d', [cb0, cb1], (_req: Request, _res: Response, next: NextFunction) => {
    console.log('the response will be sent by the next function ...')
    next()
}, (_req: Request, res: Response, next: NextFunction) => {
    res.send('Hello from D!')
    next()
}, [cb0, cb0])
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
