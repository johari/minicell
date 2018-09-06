cat <<——— > index.html
  <!doctype html>

  <head>
  <link rel="stylesheet" href="style.css" />
  <script src="elm.js"></script>
  <script src="vendor/viz/viz.js"></script>
  <script src="vendor/viz/full.render.js"></script

  </head>
  <body>
    <div id="svg-container"></div>
    <div id="elm"></div>

    <script>
      var viz = new Viz();
    </script>

    <script>
      var app = Elm.Main.init({ node: document.getElementById("elm") })
      var svgContainer = document.getElementById("svg-container");
      app.ports.repaintGraph.subscribe(function (dotOutput) {
        viz.renderSVGElement(dotOutput)
        .then(function(element) {
          svgContainer.innerHTML = "";
          svgContainer.appendChild(element);
        })
        .catch(error => {
          // Create a new Viz instance (@see Caveats page for more info)
          viz = new Viz();

          // Possibly display the error
          console.error(error);
        });
      })
    </script>


  </body>
———

elm-live src/Main.elm --output=elm.js --open
