# Introducing `jq`

First let's see what you can actually do in it.

## Case study: counting meteorites

Let's find out how many [meteorites](https://xkcd.com/1723/) fell in the Netherlands!

<details>
<summary><b>Alternative ways to follow this tutorial (Windows, web)</b></summary>
If you're on Windows, [here's](https://gitlab.ewi.tudelft.nl/cse3100/jq-clone/-/snippets/9799) a version for PowerShell.


While the best way to follow this part of the intro is with a shell open, we also put the data on jqplay.org for you to play with.
It's not quite as fast as your local installation, but works in your favourite browser: [jqplay snippet](https://jqplay.org/s/B3BM9c_xIv0).
</details>

NASA provides a database of meteorites, as JSON object, so let's download it:
    `⊢ curl "https://data.nasa.gov/resource/y77d-th95.json" > meteorites.json`

But it's pretty hard to read, since it's compressed and pretty big for a text file.
<details>
<summary>Here, judge for youself:</summary>

`⊢ cat meteorites.json | head -n 3`

```json
[{"name":"Aachen","id":"1","nametype":"Valid","recclass":"L5","mass":"21","fall":"Fell","year":"1880-01-01T00:00:00.000","reclat":"50.775000","reclong":"6.083330","geolocation":{"type":"Point","coordinates":[6.08333,50.775]}}
,{"name":"Aarhus","id":"2","nametype":"Valid","recclass":"H6","mass":"720","fall":"Fell","year":"1951-01-01T00:00:00.000","reclat":"56.183330","reclong":"10.233330","geolocation":{"type":"Point","coordinates":[10.23333,56.18333]}}
,{"name":"Abee","id":"6","nametype":"Valid","recclass":"EH4","mass":"107000","fall":"Fell","year":"1952-01-01T00:00:00.000","reclat":"54.216670","reclong":"-113.000000","geolocation":{"type":"Point","coordinates":[-113,54.21667]}}
```

```bash
⊢ du -h meteorites.json
244K    meteorites.json
```

</details>

To make it easier for us to read we can pretty-print it: `jq '.' meteorites.json`.
<details>
<summary>Output:</summary>

```bash
⊢ jq '.' meteorites.json | head -n 19
[
  {
    "name": "Aachen",
    "id": "1",
    "nametype": "Valid",
    "recclass": "L5",
    "mass": "21",
    "fall": "Fell",
    "year": "1880-01-01T00:00:00.000",
    "reclat": "50.775000",
    "reclong": "6.083330",
    "geolocation": {
      "type": "Point",
      "coordinates": [
        6.08333,
        50.775
      ]
    }
  },
```

</details>

However, this does't solve the problem with the size, so let's also select just the first object in that array: `jq '.[0]' meteorites.json`
<details>
<summary>Output:</summary>

```bash
⊢ jq '.[0]' meteorites.json
{
  "name": "Aachen",
  "id": "1",
  "nametype": "Valid",
  "recclass": "L5",
  "mass": "21",
  "fall": "Fell",
  "year": "1880-01-01T00:00:00.000",
  "reclat": "50.775000",
  "reclong": "6.083330",
  "geolocation": {
    "type": "Point",
    "coordinates": [
      6.08333,
      50.775
    ]
  }
}
```

</details>

Now that we understand what the schema is roughly, we can get to the fun part.

We can use `.field` syntax to access object fields, `.[n]` to access array elements, and pipes `op1 | op2` to chain the results of the computations.

```bash
⊢ jq '.[0] | .geolocation' meteorites.json
{
  "type": "Point",
  "coordinates": [
    6.08333,
    50.775
  ]
}
```

`jq` also includes a lot of other features, like comparisons `==`,`!=` and filters `select`.
You can check the [documentation](https://jqlang.github.io/jq/manual/) and the [tutorial](https://jqlang.github.io/jq/tutorial/11198540).

So, given a list of meteorites with all coordinates, we can list all the meteorites that fell in the Netherlands.
Of course, checking precise bounds is going to be hard, so let's just do a bounding box from [humdata.org](https://data.humdata.org/dataset/bounding-boxes-for-countries/resource/aec5d77d-095a-4d42-8a13-5193ec18a6a9):  
Latitute: from 50.75 to 53.685  
Longtitute: from 3.113 to 7.217

Then we proceed as follows:

1. Filter out the entrances without latitute and longtitude.
2. Filter out by latitute.
3. Filter out by longtitude.
4. Select the `name` field for those which satify the conditions above

```bash
⊢ jq '.[] | select (.reclat != null and .reclong != null) | select(.reclat | tonumber | (50.75 < .) and (. < 53.68)) | select (.reclong | tonumber | (3.13 < .) and (. < 7.21)) | .name' meteorites.json
```

Drumroll:

```bash
"Aachen"
"Ellemeet"
"Glanerbrug"
"Ramsdorf"
"St. Denis Westrem"
```

Our data suggests that there are at least five.
However, the name of the first one seems suspiciously German and we used a bounding box, not exact border.
And there it is, if we double-check on [https://www.lpi.usra.edu/meteor/metbull.php](https://www.lpi.usra.edu/meteor/metbull.php) it turns out that only the second and third did fall in the Netherlands (Aachen and Ramsdorf were in Germany and St. Denis Westrem in Belgium).

If you want to play with `jq` a bit more, here's a couple of things to try:

* Find a bounding box for the EU and run the same check with new boundaries.
* [https://gist.github.com/graydon/11198540](https://gist.github.com/graydon/11198540) provides bounding boxes in JSON format, write a jq filter that extracts bounding box for the Netherlands.
