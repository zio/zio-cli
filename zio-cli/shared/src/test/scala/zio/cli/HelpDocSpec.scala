package zio.cli

import zio.test.Assertion._
import zio.test._
import zio.test.{Gen, ZIOSpecDefault}

object HelpDocSpec extends ZIOSpecDefault {

  def spec = suite("HelpDocTests")(
    suite("Constructors Suite")(
      test("HelpDoc#h1 produces a Header from a String with level 1") {
        check(Gen.string) { string =>
          assert(HelpDoc.h1(string))(equalTo(HelpDoc.Header(HelpDoc.Span.Text(string), 1)))
        }
      },
      test("HelpDoc#h2 produces a Header from a String with level 2") {
        check(Gen.string) { string =>
          assert(HelpDoc.h2(string))(equalTo(HelpDoc.Header(HelpDoc.Span.Text(string), 2)))
        }
      },
      test("HelpDoc#h3 produces a Header from a String with level 3") {
        check(Gen.string) { string =>
          assert(HelpDoc.h3(string))(equalTo(HelpDoc.Header(HelpDoc.Span.Text(string), 3)))
        }
      },
      test("HelpDoc#p produces a Paragraph from a String") {
        check(Gen.string) { string =>
          assert(HelpDoc.p(string))(equalTo(HelpDoc.Paragraph(HelpDoc.Span.Text(string))))
        }
      },
      test("HelpDoc#+ combines two HelpDocs into one") {
        assert(testHeader + testParagraph)(equalTo(HelpDoc.Sequence(testHeader, testParagraph)))
      }
    ),
    suite("Boolean Suite")(
      suite("HelpDoc#isHeader: Only returns true for HelpDocs that are/start with a Header")(
        test("A Header returns true") {
          assert(testHeader.isHeader)(isTrue)
        },
        test("A Sequence beginning with a Header returns true") {
          assert(HelpDoc.Sequence(testHeader, testParagraph).isHeader)(isTrue)
        },
        test("An empty HelpDoc returns false") {
          assert(testEmpty.isHeader)(isFalse)
        },
        test("A Paragraph returns false") {
          assert(testParagraph.isHeader)(isFalse)
        },
        test("A Sequence beginning with a non-Header returns false") {
          assert(HelpDoc.Sequence(testParagraph, testHeader).isHeader)(isFalse)
        },
        test("An Enumeration beginning with a non-Header returns false") {
          assert(HelpDoc.Enumeration(List(testParagraph, testHeader, testEmpty)).isHeader)(isFalse)
        }
      ),
      suite("HelpDoc#isParagraph: Only returns true for HelpDocs that are/start with a Paragraph")(
        test("A Paragraph returns true") {
          assert(testParagraph.isParagraph)(isTrue)
        },
        test("A Sequence beginning with a Paragraph returns true") {
          assert(HelpDoc.Sequence(testParagraph, testHeader).isParagraph)(isTrue)
        },
        test("An Enumeration beginning with a Paragraph returns false") {
          assert(HelpDoc.Enumeration(List(testParagraph, testHeader, testEmpty)).isParagraph)(isFalse)
        },
        test("An empty HelpDoc returns false") {
          assert(testEmpty.isParagraph)(isFalse)
        },
        test("A Header returns false") {
          assert(testHeader.isParagraph)(isFalse)
        },
        test("A Sequence beginning with a non-Paragraph returns false") {
          assert(HelpDoc.Sequence(testHeader, testParagraph).isParagraph)(isFalse)
        },
        test("An Enumeration beginning with a non-Paragraph returns false") {
          assert(HelpDoc.Enumeration(List(testEmpty, testHeader, testParagraph)).isParagraph)(isFalse)
        }
      ),
      suite("HelpDoc#isDescriptionList: Only returns true for HelpDocs that are/start with a DescriptionList")(
        test("A DescriptionList returns true") {
          check(testDescriptionList) { s =>
            assert(s.isDescriptionList)(isTrue)
          }
        },
        test("A DescriptionList beginning with a DescriptionList returns true") {
          check(testDescriptionList, anySpan) { (desc, span) =>
            assert(HelpDoc.DescriptionList(List((span, desc))).isDescriptionList)(isTrue)
          }
        },
        test("An empty HelpDoc returns false") {
          assert(testEmpty.isDescriptionList)(isFalse)
        },
        test("A Header returns false") {
          assert(testHeader.isDescriptionList)(isFalse)
        },
        test("A Paragraph returns false") {
          assert(testParagraph.isDescriptionList)(isFalse)
        }
      ),
      suite("HelpDoc#isEnumeration: Only returns true for HelpDocs that are/start with an Enumeration")(
        test("An Enumeration returns true") {
          assert(testEnumeration.isEnumeration)(isTrue)
        },
        test("An Enumeration beginning with an Enumeration returns true") {
          assert(HelpDoc.Enumeration(List(testEnumeration, testHeader)).isEnumeration)(isTrue)
        },
        test("A Sequence beginning with an Enumeration returns true") {
          assert(HelpDoc.Sequence(testEnumeration, testHeader).isEnumeration)(isTrue)
        },
        test("An empty HelpDoc returns false") {
          assert(testEmpty.isEnumeration)(isFalse)
        },
        test("A Header returns false") {
          assert(testHeader.isEnumeration)(isFalse)
        },
        test("A Paragraph returns false") {
          assert(testParagraph.isEnumeration)(isFalse)
        }
      ),
      suite("HelpDoc#isSequence: Only returns true for HelpDocs that are/start with a Sequence")(
        test("A Sequence returns true") {
          assert(testSequence.isSequence)(isTrue)
        },
        test("A Sequence beginning with a Sequence returns true") {
          assert(HelpDoc.Sequence(testSequence, testHeader).isSequence)(isTrue)
        },
        test("An Enumeration beginning with a Sequence returns false") {
          assert(HelpDoc.Enumeration(List(testSequence, testHeader, testEmpty)).isSequence)(isFalse)
        },
        test("An empty HelpDoc returns false") {
          assert(testEmpty.isSequence)(isFalse)
        },
        test("A Header returns false") {
          assert(testHeader.isSequence)(isFalse)
        },
        test("An Enumeration beginning with a non-Sequence returns false") {
          assert(HelpDoc.Enumeration(List(testEmpty, testHeader, testSequence)).isSequence)(isFalse)
        }
      )
    ),
    suite("#is* tests")(
      test("#isEmpty returns true for an empty HelpDoc") {
        assert(HelpDoc.Empty.isEmpty)(isTrue)
      },
      test("#isEmpty returns `false` for a non-empty Header") {
        assert((testHeader.isEmpty))(isFalse)
      },
      test("Paragraph of text is not a Header") {
        assert(testParagraph.isHeader)(isFalse)
      },
      test("Sequence(Paragraph, _) starts with a Paragraph") {
        assert(HelpDoc.Sequence(testParagraph, HelpDoc.Empty).isParagraph)(isTrue)
      }
    )
  )

  val testEmpty  = HelpDoc.Empty
  val hText      = "HEADER"
  val testHeader = HelpDoc.h1(s"${hText} level 1")
  val pText      =
    """
      |Lorem ipsum dolor sit amet, consectetur adipiscing elit. Quisque facilisis sem id lacinia venenatis. 
      |Phasellus vestibulum magna eu sapien accumsan suscipit. Morbi vestibulum lobortis justo, ac pellentesque
      |tortor iaculis ut. Ut aliquet, nisl a interdum gravida, neque nisl interdum est, quis feugiat
      |ligula dui vitae dolor. Nulla pharetra ultrices justo quis tincidunt. Integer et dictum urna, eget lacinia turpis.
      |Nam mauris nibh, egestas eu tempor vel, cursus in ex. Aliquam mollis leo vel mauris semper malesuada.
      |Proin pellentesque hendrerit orci semper fermentum. Nullam eget lacinia lorem, quis venenatis nibh.
      |Phasellus risus nulla, porta et sollicitudin a, venenatis dictum orci. Pellentesque vel sagittis neque.
      |Cras elementum ligula quis vulputate posuere. Ut vulputate felis sed pellentesque suscipit. Sed quis rutrum leo.""".stripMargin
  val testParagraph   = HelpDoc.p(pText)
  val docs            = (testHeader :: testParagraph :: testEmpty :: Nil)
  val testEnumeration = HelpDoc.Enumeration(docs)
  val testSequence    = testParagraph + testEnumeration
  val anyString       = Gen.string
  val anySpan         = Gen.fromIterable(
    List(
      HelpDoc.Span.text(anyString.toString()),
      HelpDoc.Span.error(anyString.toString()),
      HelpDoc.Span.code(anyString.toString()),
      HelpDoc.Span.weak(anyString.toString()),
      HelpDoc.Span.strong(anyString.toString()),
      HelpDoc.Span.uri(java.net.URI.create(anyString.toString()))
    )
  )
  val testDescriptionList = anySpan.map(span => HelpDoc.descriptionList((span, testParagraph)))
}
