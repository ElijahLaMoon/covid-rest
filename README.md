# covid-rest

## Preface
This project may lack some general elegance which you could expect. Also, there are some missing features, like validating that input isn't empty and so on. I decided not to overcomplicate such a trivial task and stick to the most straightforward solution which came to my mind. No tests included because of the time pressure I'm currently in. During development Cabal was used as a build tool, so I recommend you to use it as well. To run the server just clone this repository and fire `cabal new-run` command in the project's root directory.

## APIs
### GET

```
GET /covid19.html
```
If no query arguments provided, returns a following template:
```html
<html>
  <head><title>Shabak COVID-19 Registration</title></head>
  <body><h1>Shabak COVID-19 Registration</h1>
    <form>
      <div>
        <label for="name">Name: </label>
        <input type="text" name="name" id="name">
      </div>
      <div>
        <label for="tel">Tel: </label>
        <input type="text" name="tel" id="tel">
      </div>
      <div>
        <input type="checkbox" name="covid19" id="covid19">
        <label for="covid19"> Tested positive for COVID-19</label>
      </div>
    <div>
      <input type="submit">
    </div>
    </form>
  </body>
</html>
```
Otherwise, expects `name`, `tel`, and (optionally) `covid19` arguments. `name` and `tel` can be any textual data, `covid19` can only have `on` value. Presence of the last argument indicates positive result from Covid test, absence, respectively, indicates negative one. For example,
```
GET /covid19.html?name=Magnus&tel=527591745
```
would return following result:
```html
<!DOCTYPE HTML>

<html>
    <head>
        <title>
            Shabak COVID-19 Registration
        </title>
    </head>
    <body>
        <h1>
            Shabak COVID-19 Registration
        </h1>
        <h2>
            Registration successful:
        </h2>
        <ul>
            <li>
                Name: Magnus
            </li>
            <li>
                Tel: 527591745
            </li>
            <li>
                Covid-19: false
            </li>
        </ul>
    </body>
</html>
```
And the one with checkbox on
```
GET /covid19.html?name=Magnus&tel=527591745
```
would return:
```html
<!DOCTYPE HTML>

<html>
    <head>
        <title>
            Shabak COVID-19 Registration
        </title>
    </head>
    <body>
        <h1>
            Shabak COVID-19 Registration
        </h1>
        <h2>
            Registration successful:
        </h2>
        <ul>
            <li>
                Name: Magnus
            </li>
            <li>
                Tel: 527591745
            </li>
            <li>
                Covid-19: true
            </li>
        </ul>
    </body>
</html>
```

### POST
```
POST /api/covid19
```

Expects JSON body with the folowing format:
```json
{
  "name": <text>,
  "tel": <text>,
  "covid19": <boolean>
}
```
And returns the same JSON with `"status": "ok"` field added.
