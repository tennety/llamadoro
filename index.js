import { Dir, readTextFile } from "tauri/api/fs";

import { Elm } from "./src/Main.elm";
import "./scss/style.scss";

readTextFile(`../static/config.json`).then(config => {
  const flags = { config: JSON.parse(config), exercises: {}};

  const app = Elm.Main.init({ flags });
})

if (module.hot) {
  module.hot.dispose(() => {
    window.location.reload();
  });
}
