package toolc
package utils

import java.io.File

case class Context(
    val reporter: Reporter,
    val file: File,
    val outDir: Option[File] = None
)
