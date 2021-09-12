package zio.config.examples

import com.github.ghik.silencer.silent
import zio.config._
import zio.system.System

import ConfigDescriptor._

object MultipleSourcesSimpleExample extends App {

  final case class MyConfig(ldap: String, port: Int, dburl: Option[String])

  // Only used to fetch the source - the pattern is only for explanation purpose
  val runtime = zio.Runtime.default

  // Assume they are different sources (env, property file, HOCON / database (in future))
  private val source1 = ConfigSource.fromMap(Map("LDAP" -> "jolap"), "constant")
  @silent("deprecated")
  private val source2 = runtime.unsafeRun(ConfigSource.fromSystemProperties)

  private val source3 = runtime.unsafeRun(ConfigSource.fromSystemEnv.provideLayer(System.live))
  private val source4 = ConfigSource.fromMap(Map("PORT" -> "1999"), "constant")
  private val source5 = ConfigSource.fromMap(Map("DB_URL" -> "newyork.com"), "constant")

  private val oneValidSource =
    ConfigSource.fromMap(
      Map(
        "LDAP"   -> "jolap",
        "PORT"   -> "1999",
        "DB_URL" -> "newyork.com"
      ),
      "constant"
    )

  // Hocon -> keys -> env
  // ConfigSource, ConfigDescriptor - mapKeys.
  // Read is lazy -
  // _.port//
  // getConfig[CaseClass]: ZIO[CaseClass] 
  // Even components within the config-data should be lazy (may be)
  // ConfigSource getValue: ZIO[PropertyTree]  
  val myConfig: ConfigDescriptor[MyConfig] =
    ((string("LDAP").from(source1).orElse(string("ldap").from(source3.at("x.y.z")))) |@| int("PORT").from(source4)) |@|
      string("DB_URL").optional.from(source1.orElse(source5)))(MyConfig.apply, MyConfig.unapply)

  val myConfig2: ConfigDescriptor[MyConfig] = 
      ((string("LDAP") |@| int("PORT")) |@|
      string("DB_URL").optional)(MyConfig.apply, MyConfig.unapply)

  val allSource = 
    source1.orElse(source3).retain("LDAP") ++ source4.retain("PORT") ++ source1.orElse(source5).retain("DB_URL")   

  // Let's reset the whole source details in the original description
  val myConfigWithReset: ConfigDescriptor[MyConfig] =
    myConfig.unsourced.from(oneValidSource) // Equivalent to myConfig.fromNothing

  // Have got a few more sources to be tried, on top of what's there already ?
  val myConfigChangedSource: ConfigDescriptor[MyConfig] = myConfig.updateSource(_.orElse(source2))

  //

  assert(
    read(myConfig) == Right(MyConfig("jolap", 1999, Some("newyork.com")))
  )

  assert(
    read(myConfigWithReset) == Right(MyConfig("jolap", 1999, Some("newyork.com")))
  )

  assert(
    read(myConfigChangedSource) == Right(MyConfig("jolap", 1999, Some("newyork.com")))
  )
}
