# askBarrel

`askBarrel` is a command line tool to querry barrelDB using command line.
It's currently read only under developpement. Basicaly a pet project to learn Haskell.

## build

```bash
stack build
```

## run

```bash
stack exec askBarrel-exe
```

### example

```bash
Welcome to ask Barrel!

   .- ¨¨¨¨ -.
  /'-.____.-'\
  '-.______.-'
 |    \ \     |
 |    /  \    |
     / /\ \
  \'-.____.-'/
   '-.____.-'
>>> :config
http://localhost:7080/dbs/mydb/
>>> :docs
Status: 200
{
    "count": 1,
    "docs": [
        {
            "doc": {
                "value": 43,
                "id": "mydoc"
            },
            "meta": {
                "rid": "AAAAAAAAAAE=",
                "rev": "5-e844ae48e74e23ef18a48db7d951aed0778fc0af9b52ef21639b3f54cbe9baaf"
            }
        }
    ]
}
>>> :doc mydoc
Status: 200
{
    "value": 43,
    "id": "mydoc"
}
>>>
```
