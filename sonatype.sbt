import xerial.sbt.Sonatype.GitHubHosting

publishMavenStyle := true
licenses := Seq("APL2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt"))
sonatypeProjectHosting := Some(GitHubHosting("udemy", "statistics", "robert.neal@udemy.com"))
developers := List(
  Developer(id="robertjneal", name="Robert J. Neal", email="robert.neal@udemy.com", url=url("https://www.udemy.com"))
)