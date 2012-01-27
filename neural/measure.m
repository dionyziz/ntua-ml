function testF1 = ...
    measure( net, TestData, TestDataTargets )
    
    testResults = sim( net, TestData );
    % size( testResults )
    % size( TestDataTargets )
    [ testAccuracy, testPrecision, testRecall ] = ...
        eval_Accuracy_Precision_Recall( testResults, TestDataTargets )
    % testF1 = 2 * testPrecision * testRecall / ( testPrecision + testRecall );