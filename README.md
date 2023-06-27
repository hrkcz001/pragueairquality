# Prague Air Quality
## Semestral project in Elm (Web-application)

- [x] ~~Main~~ About page with information about the project.
- [x] Interactive map of Prague with information about air quality in ~~each~~ several districts.
- [ ] Registration form to receive updates on the current situation in your neighborhood.
- [x] ~~News~~ Events page containing the latest information about events organized ~~in each district~~.
- Events creation as well.
- [ ] Suggestion of the best area for a promenade based on the current air quality.

## Build

`cd WebApp`

`yarn`

`npx webpack --mode production`

`cd ../PAQServer`

`stack run`

then go to `localhost:3000/refill` to inialize database
