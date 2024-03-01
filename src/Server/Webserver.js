import express from "express";
import expressWs from "express-ws";

export const createWebserver = () => {
    console.log("Creating app");
    const app = express();
    expressWs(app);
    return app;
};

export const runWebserver = ({ port }) => (app) => () => {
    app.listen(port);
};

export const addStatic = (path) => (app) => () => {
    console.log(`Adding static to ${path}`);
    const middleware = express.static(path);
    app.use(middleware);
}

export const addWebsocket = (path) => (app) => (onConnection) => () => {
    console.log(`Adding Websocket to '${path}'`);
    app.ws(path, function(ws) {
        onConnection(ws)();
    });
}
