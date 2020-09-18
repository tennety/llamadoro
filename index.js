import { Dir, readTextFile } from "tauri/api/fs";

import { Elm } from "./src/Main.elm";
import "./scss/style.scss";

const baseDirOpt = process.env.NODE_ENV === "production" ? { dir: Dir.Resource } : {}

Promise.all([
  readTextFile(`static/config.json`, baseDirOpt),
  readTextFile(`static/exercises.json`, baseDirOpt)
]).then(([config, exercises]) => {
  // For testing without Tauri
  // const config = JSON.stringify({
  //   "workInterval": 150,
  //   "shortInterval": 30,
  //   "longInterval": 60,
  //   "longBreakAfterCount": 4,
  //   "fitnessLevel": "intermediate"
  // })
  // const exercises = JSON.stringify([{
  //   "name": "Push-ups",
  //   "variations": [
  //     {
  //       "level": "beginner",
  //       "reps": 15,
  //       "directions": ["against the wall"]
  //     },
  //     {
  //       "level": "intermediate",
  //       "reps": 15,
  //       "directions": ["on the floor"]
  //     },
  //     {
  //       "level": "advanced",
  //       "reps": 25,
  //       "directions": ["on the floor"]
  //     }
  //   ]
  // }])
  const flags = { config: JSON.parse(config), exercises: JSON.parse(exercises) };

  const app = Elm.Main.init({ flags });
})

if (module.hot) {
  module.hot.dispose(() => {
    window.location.reload();
  });
}
