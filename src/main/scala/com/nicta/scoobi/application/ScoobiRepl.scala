/**
 * Copyright 2011,2012 National ICT Australia Limited
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.nicta.scoobi
package application

import org.apache.hadoop.fs._


/** A REPL for Scoobi.
  *
  * Run the 'scoobi' script, which will bring you into the Scala REPL, and then run
  * the following:
  *
  *     scala> import com.nicta.scoobi.Scoobi._
  *     scala> import com.nicta.scoobi.ScoobiRepl._
  *     scala> woof
  *
  * You're now good to go!! */
object ScoobiRepl extends ScoobiApp {

  def run() {}

  def woof() {
    configuration.jobNameIs("REPL")
    configureForCluster
    uploadLibJarsFiles(false)
    println(splash)
  }

  override lazy val upload = true

  override def jars = hadoopClasspathJars

  /** List a path . */
  def ls(path: String) {
    configuration.fileSystem.listStatus(new Path(path)) foreach { fstat =>
      Console.printf("%s%s  %-15s  %-12s  %s\n",
                     if (fstat.isDirectory) "d" else "-",
                     fstat.getPermission,
                     fstat.getOwner,
                     fstat.getBlockSize,
                     fstat.getPath.toUri.getPath)
    }
  }

  /** Get the contents of a text file. */
  def cat(path: String): Iterable[String] = {
    import scala.io._
    Source.fromInputStream(configuration.fileSystem.open(new Path(path))).getLines().toIterable
  }


  lazy val splash: String =
    """|                                                     |\
       |                                             /    /\/o\_
       |                              __    _       (.-.__.(   __o
       |       ______________  ____  / /_  (_)   /\_(      .----'
       |      / ___/ ___/ __ \/ __ \/ __ \/ /     .' \____/
       |     (__  ) /__/ /_/ / /_/ / /_/ / /     /   /  / \
       |    /____/\___/\____/\____/_.___/_/ ____:____\__\__\____________.""".stripMargin
}
