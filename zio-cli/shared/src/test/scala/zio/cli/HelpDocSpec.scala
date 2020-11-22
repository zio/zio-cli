package zio.cli

import zio.test.Assertion._
import zio.test._

object HelpDocSpec extends DefaultRunnableSpec {

  def spec = suite("HelpDocTests")(
    suite("Dumb learning test suite") (
      testM("Uppercase-ing generated strings does not yield a lowercase 'a'") {
        check(Gen.anyString) { i =>
          assert(i.toUpperCase())(not(containsString("a")))
        }
      },
      test("1 is, in fact, equal to 1") {
        assert(1)(equalTo(1))
      }
    ),
    suite("startsWithHeader") (
      test("Only returns true for `HelpDoc` that start with a `Header`") {
        assert(testHeader.isHeader)(isTrue)      &&
        assert(testParagraph.isHeader)(isFalse)  &&
        assert(testEmpty.isHeader)(isFalse)      
      }
    ),
    suite("#is* tests") (
      test("#isEmpty returns `true` for an empty HelpDoc") {
        assert(HelpDoc.Empty.isEmpty)(isTrue)
      },
      test("#isEmpty returns `false` for a non-empty Header") {
        assert((testHeader.isEmpty))(isFalse)
      },
      test("Paragraph of text is not a Header") {
        assert(testParagraph.isHeader)(isFalse)
      },
      test("Sequence(Paragraph, _) starts with a Parargraph") {
        assert(HelpDoc.Sequence(testParagraph, HelpDoc.Empty).isParagraph)(isTrue)
      },
      test("Paragraph of text is a Paragraph") {
        assert(testParagraph.isParagraph)(isTrue)
        assert(testHeader.isParagraph)(isFalse)
      }
    )
  )

  val testEmpty = HelpDoc.Empty
  val hText = "HEADER"
  val testHeader: HelpDoc = HelpDoc.Header(HelpDoc.Span.Text(s"${hText} level 1"), 1)
  val testHeader2 = HelpDoc.h2(s"${hText} level 2")
  val pText = """
    |Lorem ipsum dolor sit amet, consectetur adipiscing elit. Quisque facilisis sem id lacinia venenatis. 
    |Phasellus vestibulum magna eu sapien accumsan suscipit. Morbi vestibulum lobortis justo, ac pellentesque
    |tortor iaculis ut. Ut aliquet, nisl a interdum gravida, neque nisl interdum est, quis feugiat
    |ligula dui vitae dolor. Nulla pharetra ultrices justo quis tincidunt. Integer et dictum urna, eget lacinia turpis.
    |Nam mauris nibh, egestas eu tempor vel, cursus in ex. Aliquam mollis leo vel mauris semper malesuada.
    |Proin pellentesque hendrerit orci semper fermentum. Nullam eget lacinia lorem, quis venenatis nibh.
    |Phasellus risus nulla, porta et sollicitudin a, venenatis dictum orci. Pellentesque vel sagittis neque.
    |Cras elementum ligula quis vulputate posuere. Ut vulputate felis sed pellentesque suscipit. Sed quis rutrum leo.""".stripMargin 
  val testParagraph = HelpDoc.p(pText)
  // val testDescriptionList = ???
}


// object PrimTypeSpec extends DefaultRunnableSpec {

//   def spec = suite("PrimTypeTests")(
//     suite("Text Suite") {
//       testM("validates everything") {
//         checkM(Gen.anyString) { i =>
//           assertM(PrimType.Text.validate(i))(equalTo(i))
//         }
//       }
//     },
//     suite("Decimal Suite") {
//       testM("validate returns proper BigDecimal representation") {
//         checkM(Gen.bigDecimal(BigDecimal("1.41421356237309504880168"), BigDecimal("50.4"))) { i =>
//           assertM(PrimType.Decimal.validate(i.toString()))(equalTo(i))
//         }
//       }
//     },
//     suite("Integer Suite") {
//       testM("validate returns proper BigInt representation") {
//         checkM(anyBigIntString) { i =>
//           assertM(PrimType.Integer.validate(i))(equalTo(BigInt(i)))
//         }
//       }
//     },
//     suite("Boolean Suite")(
//       testM("validate true combinations returns proper Boolean representation") {
//         checkM(anyTrueBooleanString) { i =>
//           assertM(PrimType.Bool(None).validate(i))(equalTo(true))
//         }
//       },
//       testM("validate false combinations returns proper Boolean representation") {
//         checkM(anyFalseBooleanString) { i =>
//           assertM(PrimType.Bool(None).validate(i))(equalTo(false))
//         }
//       }
//     ),
//     suite("Instant Suite") {
//       testM("validate returns proper Instant representation") {
//         checkM(Gen.anyInstant) { i =>
//           assertM(PrimType.Instant.validate(i.toString))(equalTo(i))
//         }
//       }
//     },
//     suite("LocalDateTime Suite") {
//       testM("validate returns proper LocalDateTime representation") {
//         checkM(anyLocalDateTime) { i =>
//           assertM(PrimType.LocalDateTime.validate(i))(equalTo(LocalDateTime.parse(i)))
//         }
//       }
//     },
//     suite("LocalDate Suite") {
//       testM("validate returns proper LocalDate representation") {
//         checkM(anyLocalDate) { i =>
//           assertM(PrimType.LocalDate.validate(i))(equalTo(LocalDate.parse(i)))
//         }
//       }
//     },
//     suite("LocalTime Suite") {
//       testM("validate returns proper LocalTime representation") {
//         checkM(anyLocalTime) { i =>
//           assertM(PrimType.LocalTime.validate(i))(equalTo(LocalTime.parse(i)))
//         }
//       }
//     },
//     suite("MonthDay Suite") {
//       testM("validate returns proper MonthDay representation") {
//         checkM(anyMonthDay) { i =>
//           assertM(PrimType.MonthDay.validate(i))(equalTo(MonthDay.parse(i)))
//         }
//       }
//     },
//     suite("OffsetDateTime Suite") {
//       testM("validate returns proper OffsetDateTime representation") {
//         checkM(Gen.anyOffsetDateTime) { i =>
//           assertM(PrimType.OffsetDateTime.validate(i.toString))(equalTo(i))
//         }
//       }
//     },
//     suite("OffsetTime Suite") {
//       testM("validate returns proper OffsetTime representation") {
//         checkM(Gen.anyOffsetDateTime.map(_.toOffsetTime)) { i =>
//           assertM(PrimType.OffsetTime.validate(i.toString))(equalTo(i))
//         }
//       }
//     },
//     suite("Year Suite") {
//       testM("validate returns proper Year representation") {
//         checkM(anyYear) { i =>
//           assertM(PrimType.Year.validate(i.toString))(equalTo(i))
//         }
//       }
//     },
//     suite("YearMonth Suite") {
//       testM("validate returns proper YearMonth representation") {
//         checkM(anyYearMonth) { i =>
//           assertM(PrimType.YearMonth.validate(i.toString))(equalTo(i))
//         }
//       }
//     }
//   )

//   val anyBigIntString                         = Gen.long(0, Long.MaxValue).map(BigInt(_)).map(_.toString)
//   val anyTrueBooleanString: Gen[Any, String]  = Gen.fromIterable(List("true", "TruE", "1", "y", "yes", "yEs", "on"))
//   val anyFalseBooleanString: Gen[Any, String] = Gen.fromIterable(List("false", "FAlSE", "0", "n", "no", "off", "OFF"))

//   val anyInstant       = Gen.anyInstant.map(_.atZone(ZoneOffset.UTC))
//   val anyLocalDateTime = anyInstant.map(_.toLocalDateTime.toString)
//   val anyLocalDate     = anyInstant.map(_.toLocalDate.toString)
//   val anyLocalTime     = anyInstant.map(_.toLocalTime.toString)
//   val anyMonthDay      = anyInstant.map(d => MonthDay.of(d.getMonthValue, d.getDayOfMonth).toString)
//   val anyYear          = Gen.int(Year.MIN_VALUE, Year.MAX_VALUE).map(Year.of)
//   val anyYearMonth     = anyYear.map(d => YearMonth.of(d.getValue(), 2))

// }