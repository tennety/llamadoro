import { Dir, readTextFile } from "tauri/api/fs";

import { Elm } from "./src/Main.elm";
import "./scss/style.scss";

readTextFile(`../static/config.json`).then(config => {
  // For testing without Tauri
  // const config = JSON.stringify({
  //   "workInterval": 1560,
  //   "shortInterval": 360,
  //   "longInterval": 660,
  //   "longBreakAfterCount": 5
  // })
  const flags = { config: JSON.parse(config), exercises: {}};

  const app = Elm.Main.init({ flags });
})

if (module.hot) {
  module.hot.dispose(() => {
    window.location.reload();
  });
}
