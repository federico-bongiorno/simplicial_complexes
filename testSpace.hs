import Space


main = do

	-- HOMOLOGY -----------------------------------------------------------------------------------

	-- Test 1. This should be [1]
	putStrLn $ "Test 01. Homology of point: " ++ (show homologyPoint)
	putStrLn $ "         Test Outcome: " ++ printResult( homologyPoint == [1] )

	-- Test 2. This should be [1,0]
	putStrLn $ "Test 02. Homology of tree: " ++ (show homologyTree)
	putStrLn $ "         Test Outcome: " ++ printResult( homologyTree == [1,0] )

	-- Test 3. This should be [1,6]
	putStrLn $ "Test 03. Homology of hexagon: " ++ (show homologyHexagon)
	putStrLn $ "         Test Outcome: " ++ printResult( homologyHexagon == [1,6] )

	-- Test 4. This should be [2,1,0]
	putStrLn $ "Test 04. Homology of disjoint: " ++ (show homologyDisjoint)
	putStrLn $ "         Test Outcome: " ++ printResult( homologyDisjoint == [2,1,0] )

	-- Test 5. This should be [1,0,0,0,0,0,0,0,0,0]
	putStrLn $ "Test 05. Homology of 9-simplex: " ++ (show homologySimplex)
	putStrLn $ "         Test Outcome: " ++ printResult( homologySimplex == [1,0,0,0,0,0,0,0,0,0] )

	-- Test 6. This should be [1,0,1]
	putStrLn $ "Test 06. Homology of 2-sphere: " ++ (show homologySphere)
	putStrLn $ "         Test Outcome: " ++ printResult( homologySphere == [1,0,1] )

	-- Test 7. This should be [1,2,1]
	putStrLn $ "Test 07. Homology of torus: " ++ (show homologyTorus)
	putStrLn $ "         Test Outcome: " ++ printResult( homologyTorus == [1,2,1] )

	-- Test 8. This should be [1,1,0]
	putStrLn $ "Test 08. Homology of klein bottle: " ++ (show homologyBottle)
	putStrLn $ "         Test Outcome: " ++ printResult( homologyBottle == [1,1,0] )

	-- Test 9. This should be [1,0,0]. Unfortunately rational homology cannot see torsion
	putStrLn $ "Test 09. Homology of real projective plane: " ++ (show homologyPlane)
	putStrLn $ "         Test Outcome: " ++ printResult( homologyPlane == [1,0,0] )





	-- DIMENSION ----------------------------------------------------------------------------------

	-- Test 10. This should be 0
	putStrLn $ "Test 10. Dimension of point: " ++ (show dimensionPoint)
	putStrLn $ "         Test Outcome: " ++ printResult( dimensionPoint == 0 )

	-- Test 11. This should be 9
	putStrLn $ "Test 11. Dimension of 9-simplex: " ++ (show dimensionSimplex)
	putStrLn $ "         Test Outcome: " ++ printResult( dimensionSimplex == 9 )

	-- Test 12. This should be 2
	putStrLn $ "Test 12. Dimension of disjoint: " ++ (show dimensionDisjoint)
	putStrLn $ "         Test Outcome: " ++ printResult( dimensionDisjoint == 2 )





	-- EULER CHARACTERISTIC -----------------------------------------------------------------------

	-- Test 13. This should be 2
	putStrLn $ "Test 13. Euler Characteristic of 2-shere: " ++ (show eulerSphere)
	putStrLn $ "         Test Outcome: " ++ printResult( eulerSphere == 2 )

	-- Test 14. This should be 0
	putStrLn $ "Test 14. Euler Characteristic of torus: " ++ (show eulerTorus)
	putStrLn $ "         Test Outcome: " ++ printResult( eulerTorus == 0 )

	-- Test 15. This should be 1
	putStrLn $ "Test 15. Euler Characteristic of real projective plane: " ++ (show eulerPlane)
	putStrLn $ "         Test Outcome: " ++ printResult( eulerPlane == 1 )





	-- CONNECTEDNESS ------------------------------------------------------------------------------
    
    -- Test 16. This is not connected
	putStrLn $ "Test 16. Is the disjoint connected: " ++ (show conDisjoint)
	putStrLn $ "         Test Outcome: " ++ printResult( conDisjoint == False )

	-- Test 17. This is connected
	putStrLn $ "Test 17. Is the 9-simplex connected: " ++ (show conSimplex)
	putStrLn $ "         Test Outcome: " ++ printResult( conSimplex == True )

	-- Test 18. I don't know what this should be
	putStrLn $ "Test 18. Is the klein bottle connected : " ++ (show conBottle)
	putStrLn $ "         Test Outcome: " ++ printResult( conBottle == True )





	-- CONTRACTIBLE -------------------------------------------------------------------------------
    
	-- Test 19. This is contractible
	putStrLn $ "Test 19. Could the tree be contractible: " ++ (show contractTree)
	putStrLn $ "         Test Outcome: " ++ printResult( contractTree == True )

	-- Test 20. This is not contractible
	putStrLn $ "Test 20. Could the hexagon be contractible: " ++ (show contractHexagon)
	putStrLn $ "         Test Outcome: " ++ printResult( contractHexagon == False )

	-- Test 21. This is not contractible. However rational homology cannot see that
	putStrLn $ "Test 21. Could the real projective plane be contractible: " ++ (show contractPlane)
	putStrLn $ "         Test Outcome: " ++ printResult( contractPlane == True )





	-- HOMOTOPIC ----------------------------------------------------------------------------------
    
    -- Test 22. This is shown in a first course in algebraic topology. The answer is no
	putStrLn $ "Test 22. Could the 2-sphere and the torus be homotopic: " ++ (show homSphereTorus)
	putStrLn $ "         Test Outcome: " ++ printResult( homSphereTorus == False )

	-- Test 23. This is indeed true
	putStrLn $ "Test 23. Could the point and the 9-simplex be homotopic: " ++ (show homPointSimplex)
	putStrLn $ "         Test Outcome: " ++ printResult( homPointSimplex == True )

	-- Test 24. I don't know what this should be
	putStrLn $ "Test 24. Could the klein bottle and the projective plane be homotopic: " ++ (show homBottlePlane)
	putStrLn $ "         Test Outcome: " ++ printResult( homBottlePlane == False )





	-- POINCARÉ -----------------------------------------------------------------------------------
    
	-- Test 25. This is false
	putStrLn $ "Test 25. Does rational Poincaré duality hold true for the 9-simplex: " ++ (show dualSimplex)
	putStrLn $ "         Test Outcome: " ++ printResult( dualSimplex == False )

	-- Test 26. This is true
	putStrLn $ "Test 26. Does rational Poincaré duality hold true for the 2-sphere: " ++ (show dualSphere)
	putStrLn $ "         Test Outcome: " ++ printResult( dualSphere == True )

	-- Test 27. This is false
	putStrLn $ "Test 27. Does rational Poincaré duality hold true for the klein bottle: " ++ (show dualBottle)
	putStrLn $ "         Test Outcome: " ++ printResult( dualBottle == False )





    -- CALCULATIONS -------------------------------------------------------------------------------

	    where printResult what = if what == True then "Pass.\n" else "Fail.\n"

	          homologyPoint = homology point
	          homologyTree = homology tree
	          homologyHexagon = homology hexagon 
	          homologyDisjoint = homology disjoint
	          homologySimplex = homology simplex
	          homologySphere = homology sphere
	          homologyTorus = homology torus
	          homologyBottle = homology kleinBottle
	          homologyPlane = homology projectivePlane

	          dimensionPoint = dimension point
	          dimensionSimplex = dimension simplex
	          dimensionDisjoint = dimension disjoint

	          eulerSphere = eulerCharacteristic sphere
	          eulerTorus = eulerCharacteristic torus
	          eulerPlane = eulerCharacteristic projectivePlane

	          conDisjoint = isConnected disjoint
	          conSimplex = isConnected simplex
	          conBottle = isConnected kleinBottle

	          contractTree = couldItBeContractible tree
	          contractHexagon = couldItBeContractible hexagon
	          contractPlane = couldItBeContractible projectivePlane

	          homSphereTorus = couldTheyBeHomotopic sphere torus
	          homPointSimplex = couldTheyBeHomotopic point simplex
	          homBottlePlane = couldTheyBeHomotopic kleinBottle projectivePlane

	          dualSimplex = rationalPoincareDuality simplex
	          dualSphere = rationalPoincareDuality sphere
	          dualBottle = rationalPoincareDuality kleinBottle





	-- SPACES -------------------------------------------------------------------------------------
	          
	          point = Space (allSimplices [[1]])

        	  -- A contractible graph, i.e. a tree
	    	  tree = Space (allSimplices [[1,2],[1,3],[1,4],[4,5],[4,8],[4,6],[6,7]])

	    	  -- Skeleton of six equilateral triangles with a common vertex
	    	  hexagon = Space (allSimplices [[0,1],[0,2],[0,3],[0,4],[0,5],[0,6],[1,2],[2,3],[3,4],[4,5],[5,6],[6,1]])

	    	  -- Two connected components. One is the skeleton of a triangle, the other a full triangle
	    	  disjoint = Space (allSimplices [['a','b'],['b','c'],['c','a'],['d','e'],['e','f'],['f','d'],['d','e','f']])

	    	  -- 9 dimensional simplex
	    	  simplex = Space (allSimplices [[0,1,2,3,4,5,6,7,8,9]])

	    	  sphere = Space (allSimplices [[1,2,3],[1,3,4],[1,4,5],[1,2,5],[2,3,4],[2,4,5]])

	    	  torus = Space (allSimplices [[1,2,4],[2,4,6],[2,3,6],[3,6,7],[3,7,1],[1,7,4],[4,5,6],[6,5,8],[6,7,8],[7,8,9],[4,7,9],[4,5,9],[8,5,1],[1,2,8],[8,9,2],[2,3,9],[3,5,9],[1,3,5]])

	    	  kleinBottle = Space (allSimplices [[1,2,4],[2,4,6],[2,3,6],[3,6,7],[3,7,1],[1,7,4],[4,5,6],[6,5,8],[6,7,8],[7,8,9],[4,7,9],[4,5,9],[8,5,1],[1,3,8],[8,9,3],[2,3,9],[2,5,9],[1,2,5]])

	    	  projectivePlane = Space (allSimplices [[1,4,5],[1,3,4],[2,3,4],[1,2,6],[1,3,6],[2,3,5],[1,2,5],[4,5,6],[2,4,6],[3,5,6]])