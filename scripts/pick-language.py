import os
import json
import random
from typing import List, Dict, Tuple


LANGUAGE_JSON_FILEPATH = "languages.json"
PICKED_JSON_FILEPATH = "picked.json"


def read_languages() -> Tuple[List[str], Dict[str, str]]:
    with open(LANGUAGE_JSON_FILEPATH) as file:
        data = json.load(file)
        languages = data["languages"]
        file_exts = data["extensions"]
        return languages, file_exts


def read_picked():
    if not os.path.exists(PICKED_JSON_FILEPATH):
        with open(PICKED_JSON_FILEPATH, "w+") as file:
            file.write("[]")
    with open(PICKED_JSON_FILEPATH, "r") as file:
        picked = list(json.load(file))
        return picked


def pick_language(languages: List[str], picked: List[str]):
    pickable = list(set(languages) - set(picked))
    return random.choice(pickable)


def create_folder(day: int, language: str, extension: str):
    # create the folder
    folder_path = f"day{day:02}_{language.lower()}"
    os.mkdir(folder_path)

    # create files in the new folder
    solution_path = os.path.join(folder_path, f"solution.{extension}")
    run_script_path = os.path.join(folder_path, "run.sh")
    input_path = os.path.join(folder_path, "input.txt")
    open(solution_path, "w").close()
    open(run_script_path, "w").close()
    open(input_path, "w").close()


def save_picked(picked: List[str]):
    with open(PICKED_JSON_FILEPATH, "w") as file:
        json.dump(picked, file)


if __name__ == "__main__":
    languages, file_exts = read_languages()
    picked = read_picked()
    lang = pick_language(languages, picked)
    picked.append(lang)
    save_picked(picked)
    create_folder(len(picked))