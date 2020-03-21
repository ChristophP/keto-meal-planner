/* eslint-disable no-restricted-globals, no-underscore-dangle */
const manifest = self.__precacheManifest;
const cacheName = `precache-version-${manifest.ver}`;

const precache = () =>
  caches
    .open(cacheName)
    .then(cache => cache.addAll(manifest.files))
    .then(() => self.skipWaiting());

const cleanCache = () =>
  caches
    .keys()
    .then(cacheNames =>
      Promise.all(
        cacheNames.map(name =>
          name !== cacheName ? caches.delete(name) : Promise.resolve()
        )
      )
    )
    .then(() => self.clients.claim());

const handleRequests = request =>
  caches.open(cacheName).then(cache => {
    const newRequest = request.mode === "navigate" ? "/" : request;
    return fetch(newRequest)
      .then(response => {
        cache.put(newRequest, response.clone());
        return response;
      })
      .catch(() => cache.match(newRequest));
  });

// install and precache
self.addEventListener("install", event => {
  event.waitUntil(precache());
});

// remove old caches and claim clients
self.addEventListener("activate", event => {
  event.waitUntil(cleanCache());
});

// handle requests
self.addEventListener("fetch", event => {
  event.respondWith(handleRequests(event.request));
});
