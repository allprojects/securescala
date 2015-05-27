package crypto

import org.scalameter.api._

// Add DSV reporter
trait CustomPerformanceTest extends PerformanceTest.OfflineReport {
  override def reporter: Reporter = Reporter.Composite(
    new RegressionReporter(tester, historian),
    HtmlReporter(!online),
    DsvReporter(','),
    ChartReporter(ChartFactory.XYLine())
  )
}
