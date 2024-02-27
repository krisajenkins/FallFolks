import RobustWebSocket from 'robust-websocket';

export const makeWebsocketClient = (websocketClientConfig) => () => {
    return new RobustWebSocket(websocketClientConfig.url);
};

export const onmessage = (websocketClient) => (handler) => () => {
    websocketClient.onmessage = (msg) => {
        handler(msg.data)();
    };
};
export const send = (websocketClient) => (msg) => () => {
    websocketClient.send(msg);
}
