/* eslint-disable no-restricted-globals, no-underscore-dangle */
const manifest = self.__precacheManifest;
const cacheName = `precache-version-${manifest.ver}`;

// install and precache
self.addEventListener("install", event => {
  event.waitUntil(
    caches
      .open(cacheName)
      .then(cache => {
        return cache.addAll(manifest.files);
      })
      .then(() => self.skipWaiting())
  );
});

// remove old caches and claim clients
self.addEventListener("activate", event => {
  event.waitUntil(
    caches
      .keys()
      .then(cacheNames => {
        return Promise.all(
          cacheNames.map(name =>
            name !== cacheName ? caches.delete(name) : Promise.resolve()
          )
        );
      })
      .then(() => self.clients.claim())
  );
});

// handle requests
self.addEventListener("fetch", event => {
  event.respondWith(
    caches.open(cacheName).then(cache => {
      const request = event.request.mode === "navigate" ? "/" : event.request;
      return fetch(request)
        .then(response => {
          cache.put(event.request, response.clone());
          return response;
        })
        .catch(() => cache.match(request));
    })
  );
});
