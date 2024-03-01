export const onMessage = (websocketConnection) => (handler) => () => {
  websocketConnection.onmessage = (msg) = handler(msg.data)();
};

export const send = (websocketConnection) => (msg) => () => {
  websocketConnection.send(msg);
};
