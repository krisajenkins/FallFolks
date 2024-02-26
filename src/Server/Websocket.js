import { WebSocketServer } from "ws";

export const createServer = (websocketServerConfig) => () => {
    return new WebSocketServer(websocketServerConfig);
};

export const onConnection = (websocketServer) => (handler) => () => {
    websocketServer.on("connection", function(websocketConnection) {
        handler(websocketConnection)();
    });
};

export const onMessage = (websocketConnection) => (handler) => () => {
    websocketConnection.onmessage = function(msg) {
        handler(msg.data)();
    };
};

export const send = (websocketConnection) => (msg) => () => {
    websocketConnection.send(msg);
};

// ws.on("message", function message(data) {
// console.log("Handling.", websocketConnection);
// console.log("Handling.", websocketConnection);
// console.log("Handling.", websocketConnection);
//   console.log("received: %s", data);
// });

// ws.send("something");
