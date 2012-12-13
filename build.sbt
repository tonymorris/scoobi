/** Definition */
name := "scoobi"

organization := "com.nicta"

version := "0.6.0-cdh4-SNAPSHOT"

scalaVersion := "2.9.2"

libraryDependencies ++= Seq(
  "javassist" % "javassist" % "3.12.1.GA",
  "org.apache.avro" % "avro-mapred" % "1.7.3-SNAPSHOT",
  "org.apache.avro" % "avro" % "1.7.3-SNAPSHOT",
  "org.apache.hadoop" % "hadoop-client" % "2.0.0-mr1-cdh4.0.1",
  "org.apache.hadoop" % "hadoop-core" % "2.0.0-mr1-cdh4.0.1",
  "com.thoughtworks.xstream" % "xstream" % "1.4.3" intransitive(),
  "org.scalaz" %% "scalaz-core" % "7.0.0-M3",
  "org.specs2" %% "specs2" % "1.12.3" % "optional",
  "com.chuusai" %% "shapeless" % "1.2.2",
  "org.specs2" % "classycle" % "1.4.1"% "test",
  "org.scalacheck" %% "scalacheck" % "1.9" % "test",
  "org.scala-tools.testing" % "test-interface" % "0.5" % "test",
  "org.hamcrest" % "hamcrest-all" % "1.1" % "test",
  "org.mockito" % "mockito-all" % "1.9.0" % "optional",
  "org.pegdown" % "pegdown" % "1.0.2" % "test",
  "junit" % "junit" % "4.7" % "test",
  "org.apache.commons" % "commons-math" % "2.2" % "test",
  "org.apache.commons" % "commons-compress" % "1.0" % "test",
  "org.apache.pig" % "pig" % "0.10.0" % "test"
)

(sourceGenerators in Compile) <+= (sourceManaged in Compile) map GenWireFormat.gen

resolvers ++= Seq("nicta's avro" at "http://nicta.github.com/scoobi/releases",
                  "cloudera" at "https://repository.cloudera.com/content/repositories/releases",
                  "sonatype" at "http://oss.sonatype.org/content/repositories/snapshots")

/** Compilation */
scalacOptions ++= Seq("-deprecation", "-Ydependent-method-types", "-unchecked")

/** Testing */
testOptions := Seq(Tests.Filter(s => s.endsWith("Spec") ||
                                     Seq("Index", "All", "UserGuide", "ReadMe").exists(s.contains)))

fork in Test := true

javaOptions ++= Seq("-Djava.security.krb5.realm=OX.AC.UK",
                    "-Djava.security.krb5.kdc=kdc0.ox.ac.uk:kdc1.ox.ac.uk")

publishArtifact in packageDoc := false // disable building docs, as it takes so much time

pomExtra :=
    <build>
        <plugins>
             <plugin>
                <groupId>com.mycila.maven-license-plugin</groupId>
                <artifactId>maven-license-plugin</artifactId>
                <configuration>
                    <header>notes/header.txt</header>
                </configuration>
            </plugin>
        </plugins>
    </build>

