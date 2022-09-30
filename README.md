<div id="top"></div>
<!--
*** Credit for this amazing README template goes to @othneildrew#59
*** THank you for your hard work!
-->



<!-- PROJECT SHIELDS -->
<!--
*** I'm using markdown "reference style" links for readability.
*** Reference links are enclosed in brackets [ ] instead of parentheses ( ).
*** See the bottom of this document for the declaration of the reference
*** variables for contributors-url, forks-url, etc. This is an optional,
*** concise syntax you may use.
*** https://www.markdownguide.org/basic-syntax/#reference-style-links
-->
[![Contributors][contributors-shield]][contributors-url]
[![Forks][forks-shield]][forks-url]
[![Stargazers][stars-shield]][stars-url]
[![Issues][issues-shield]][issues-url]
[![MIT License][license-shield]][license-url]
[![LinkedIn][linkedin-shield]][linkedin-url]


<h3 align="center">NFL Elo Rating Visualization</h3>

  <p align="left">
    This project sources Elo Ratings from FiveThirtyEight for National Football
    League teams and Quarterbacks and uses the R Shiny platform to provide a
    concise and useful user interface to help others explore the data.
    <br />
    <a href="https://github.com/JTG89/nfl_elo_ratings"><strong>Explore the docs »</strong></a>
    <br />
    <br />
    <a href="https://github.com/JTG89/nfl_elo_ratings">View Demo</a>
    ·
    <a href="https://github.com/JTG89/nfl_elo_ratings/issues">Report Bug</a>
    ·
    <a href="https://github.com/JTG89/nfl_elo_ratings/issues">Request Feature</a>
  </p>
</div>



<!-- TABLE OF CONTENTS -->
<details>
  <summary>Table of Contents</summary>
  <ol>
    <li>
      <a href="#about-the-project">About The Project</a>
      <ul>
        <li><a href="#built-with">Built With</a></li>
      </ul>
    </li>
    <li>
      <a href="#getting-started">Getting Started</a>
      <ul>
        <li><a href="#prerequisites">Prerequisites</a></li>
        <li><a href="#installation">Installation</a></li>
      </ul>
    </li>
    <li><a href="#usage">Usage</a></li>
    <li><a href="#roadmap">Roadmap</a></li>
    <li><a href="#contributing">Contributing</a></li>
    <li><a href="#license">License</a></li>
    <li><a href="#contact">Contact</a></li>
    <li><a href="#acknowledgments">Acknowledgments</a></li>
  </ol>
</details>



<!-- ABOUT THE PROJECT -->
## About The Project

Elo Ratings are a method for calculating the relative skill levels in a
zero-sum game such as chess, or in this case, American football.  This rating
system was created by Arpad Elo, a Hungarian-American physics professor, and
the creator of the system.

Elo ratings were originally developed for use in competitive chess, but has
since been applied as a rating system in American football, baseball,
basketball, billiards, table tennis, and many other competitive events such
as e-sports.  This particular implementation focuses on the National Football
League (NFL) and Elo ratings of its member teams since the beginning of the
Super Bowl Era, which began with the 1966 regular season.

Elo ratings are a zero-sum system, with the victorious team "winning" a
number of points from the losing team after every game.  No additional points
are ever created or destroyed.  The ratings of the teams prior to the game
determine how many points are won by the victor and surrendered by the loser.
A winner who had a higher rating entering the game will earn fewer points from
a weak opponent than would a lower ranked winner which beat a team with a
higher ranking.  Elo ratings are also not considered an "absolute" metric of
skill, meaning that they have no meaning outside the contained pool of teams
and games (i.e., each season).  Therefore, ratings between various years cannot
necessarily be fairly compared.

The data used to develop this project is sourced from FiveThirtyEight and their
excellent [The Complete History Of The NFL](https://projects.fivethirtyeight.com/complete-history-of-the-nfl/)
project.  The data contains the results of every NFL game played since the
beginning of the 1966 regular season, and is current through Super Bowl LV
(55), played in February of 2021 (2021 season data will be added when it is
available on FiveThirtyEight's GitHub page).

<!--[![Product Name Screen Shot][product-screenshot]](https://example.com)-->

<p align="right">(<a href="#readme-top">back to top</a>)</p>

### Built With

* [![R][R]][R_url]
* [![Shiny][Shiny]][Shiny_url]

<p align="right">(<a href="#readme-top">back to top</a>)</p>

<!-- GETTING STARTED -->
## Getting Started

To generate your own copy of the Elo rating visualization tool, you'll need
copies of the `global.R`, `server.R` and `ui.R` file from this repo, which
can then be run using R Studio or any R-supported IDE.

### Prerequisites

To work with the scripts required to run the Elo rating visualization tool,
you will need to download R from the project website and R Studio (soon to be
re-branded as Posit) from its project site as well.  The links to each are
provided below.  R Studio is required because Shiny is a direct product of R
Studio and the links between the `global.R`, `server.R`, and `ui.R` files are
not inherently understood by the base R language.

* R Statistical Programming Language
  ```sh
  https://www.r-project.org/
  ```

* R Studio
  ```sh
  https://www.rstudio.com/products/rstudio/download/
  ```

### Installation

1. Clone the repo
   ```sh
   git clone https://github.com/JTG89/nfl_elo_ratings.git
   ```
2. Create a local repo to run the files locally

3. Open either `ui.R` or `server.R` and select the `Run App` button at the
  top-right of the IDE to start the visualization tool.


<p align="right">(<a href="#readme-top">back to top</a>)</p>


<!-- ROADMAP -->
## Roadmap

- [ ] Feature 1
    - [ ] Nested Feature

See the [open issues](https://github.com/JTG89/nfl_elo_ratings/issues) for a
full list of proposed features (and known issues).

<p align="right">(<a href="#readme-top">back to top</a>)</p>



<!-- CONTRIBUTING -->
## Contributing

Contributions are what make the open source community such an amazing place to
learn, inspire, and create. Any contributions you make are **greatly**
**appreciated**.

If you have a suggestion that would make this better, please fork the repo and
create a pull request. You can also simply open an issue with the tag
"enhancement".
Don't forget to give the project a star if you found it enjoyable! And thank you!

1. Fork the Project
2. Create your Feature Branch (`git checkout -b feature/AmazingFeature`)
3. Commit your Changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the Branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

<p align="right">(<a href="#readme-top">back to top</a>)</p>



<!-- LICENSE -->
## License

Distributed under the MIT License. See `LICENSE.txt` for more information.

<p align="right">(<a href="#readme-top">back to top</a>)</p>



<!-- CONTACT -->
## Contact

Your Name - jgamber89@gmail.com

Project Link: [https://github.com/JTG89/nfl_elo_ratings](https://github.com/JTG89/nfl_elo_ratings)

<p align="right">(<a href="#readme-top">back to top</a>)</p>



<!-- ACKNOWLEDGMENTS -->
## Acknowledgments

I could not have begun this project without the teaching and encouragement from
my professor, Dr. Joe Yurko at the University of Pittsburgh

<p align="right">(<a href="#readme-top">back to top</a>)</p>



<!-- MARKDOWN LINKS & IMAGES -->
<!-- https://www.markdownguide.org/basic-syntax/#reference-style-links -->
[contributors-shield]: https://img.shields.io/github/contributors/JTG89/nfl_elo_ratings.svg?style=for-the-badge
[contributors-url]: https://github.com/JTG89/nfl_elo_ratings/graphs/contributors
[forks-shield]: https://img.shields.io/github/forks/JTG89/nfl_elo_ratings.svg?style=for-the-badge
[forks-url]: https://github.com/JTG89/nfl_elo_ratings/network/members
[stars-shield]: https://img.shields.io/github/stars/JTG89/nfl_elo_ratings.svg?style=for-the-badge
[stars-url]: https://github.com/JTG89/nfl_elo_ratings/stargazers
[issues-shield]: https://img.shields.io/github/issues/JTG89/nfl_elo_ratings.svg?style=for-the-badge
[issues-url]: https://github.com/JTG89/nfl_elo_ratings/issues
[license-shield]: https://img.shields.io/github/license/JTG89/nfl_elo_ratings.svg?style=for-the-badge
[license-url]: https://github.com/JTG89/nfl_elo_ratings/blob/master/LICENSE.txt
[linkedin-shield]: https://img.shields.io/badge/-LinkedIn-black.svg?style=for-the-badge&logo=linkedin&colorB=555
[linkedin-url]: https://linkedin.com/in/linkedin_username
[product-screenshot]: images/screenshot.png
[R]: https://img.shields.io/badge/R-000000?style=for-the-badge&logo=R&logoColor=blue
[R_url]: https://www.r-project.org/
[Shiny]: https://img.shields.io/badge/Shiny-5E9EDA?style=for-the-badge&logo=shiny&logoColor=gray
[Shiny_url]: https://shiny.rstudio.com/