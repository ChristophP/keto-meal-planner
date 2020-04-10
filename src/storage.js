const get = (key) => Promise.resolve(window.localStorage.getItem(key));
const set = (key, value) => Promise.resolve(window.localStorage.setItem(key, value))

export {get, set}

