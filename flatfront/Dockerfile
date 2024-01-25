FROM node:lts AS runtime
WORKDIR /app

COPY . .

RUN npm install
RUN npm run build

RUN npm install --global http-server

# ENV HOST=0.0.0.0
# ENV PORT=4321
# EXPOSE 4321
# CMD node ./dist/server/entry.mjs