object Puissance4 {

    import scala.util.Random
    
    // définition du type "plateau" : tableau de tableau d'entiers
    type Plateau = Array[Array[Int]]

    // fonction qui demande à l'utilisateur de saisir un entier tant que sa saisie est incorrecte. Renvoie ensuite cet entier
    def saisieEntier : Int = {
        var chaine: String = scala.io.StdIn.readLine()
        (chaine.length) match {
            case (0) => {
                println("Valeur saisie incorrecte. Recommencer : ")
                saisieEntier
            }
            case (_) => {
                try {
                    chaine.toInt
                } catch {
                    case e: Exception => {
                        println("Valeur saisie incorrecte. Recommencer : ")
                        saisieEntier
                    }
                }
            }
        }
    }

    // fonction qui renvoie le nombre de colonnes saisi par l'utilisateur si et seulement celui-ci est strictement supérieur à 6. Redemande à l'utilisateur sinon
    def saisieNbColonnes(): Int = {
        var nombre: Int = saisieEntier
        if (nombre < 7) {
            print("Erreur : nombre de colonnes strictement inférieur à 7. Recommencer : ")
            saisieNbColonnes
        } else {
            nombre
        }
    }

    // fonction qui renvoie le nombre de lignes saisi par l'utilisateur si et seulement celui-ci est strictement supérieur à 6. Redemande à l'utilisateur sinon
    def saisieNbLignes(): Int = {
        var nombre: Int = saisieEntier
        if (nombre < 6) {
            print("Erreur : nombre de lignes strictement inférieur à 6. Recommencer : ")
            saisieNbLignes
        } else {
            nombre
        }
    }

    // fonction qui renvoie le plateau dont l'élément "plateau(numColonne)(numLigne)" a été remplacé par "element"
    def remplaceElemPlateau(plateau: Plateau, numColonne: Int, numLigne: Int, element: Int): Plateau = {
        plateau(numColonne)(numLigne) = element
        plateau
    }

    // (sous-fonction de récursion de la fonction "importPlateau()")
     def importPlateauRT(plateau: Plateau, chaineSplit: Array[String], numColonne: Int, numLigne: Int, numColMax: Int, numLigMax: Int): Plateau = {
        (numColonne, numLigne, chaineSplit(numLigne)(numColonne)) match {
            case (`numColMax`, `numLigMax`, 'J') => remplaceElemPlateau(plateau, numColMax, 0, 1)
            case (`numColMax`, `numLigMax`, 'R') => remplaceElemPlateau(plateau, numColMax, 0, 2)
            case (`numColMax`, `numLigMax`, _) => remplaceElemPlateau(plateau, numColMax, 0, 0)
            case (`numColMax`, _, 'J') => importPlateauRT(remplaceElemPlateau(plateau, numColMax, numLigMax-numLigne, 1), chaineSplit, 0, numLigne+1, numColMax, numLigMax)
            case (`numColMax`, _, 'R') => importPlateauRT(remplaceElemPlateau(plateau, numColMax, numLigMax-numLigne, 2), chaineSplit, 0, numLigne+1, numColMax, numLigMax)
            case (`numColMax`, _, _) => importPlateauRT(remplaceElemPlateau(plateau, numColMax, numLigMax-numLigne, 0), chaineSplit, 0, numLigne+1, numColMax, numLigMax)
            case (_, _, 'J') => importPlateauRT(remplaceElemPlateau(plateau, numColonne, numLigMax-numLigne, 1), chaineSplit, numColonne+1, numLigne, numColMax, numLigMax)
            case (_, _, 'R') => importPlateauRT(remplaceElemPlateau(plateau, numColonne, numLigMax-numLigne, 2), chaineSplit, numColonne+1, numLigne, numColMax, numLigMax)
            case (_, _, _) => importPlateauRT(remplaceElemPlateau(plateau, numColonne, numLigMax-numLigne, 0), chaineSplit, numColonne+1, numLigne, numColMax, numLigMax)
        }
    }

    // fonction qui renvoie un plateau construit à partir d'une chaine de caractères "chaine". Dans la "chaine", le séparateur des lignes du plateau à construire doit être ' ', les jetons doivent être 'J' ou 'R', et les cases vides doivent être '.'
    // exemple d'utilisation : "import("RJJJRJR JRJRRJR JRJRJJ. .R.R... ....... .......")" renvoie le plateau de la page 2 de l'énoncé du projet
    def importPlateau(chaine: String): Plateau = {
        if (chaine.length <= 0) {
            println("Chaine trop courte : plateau vide par défaut de 6*7 créé.")
            Array.ofDim[Int](7,6)
        } else {
            var chaineSplit: Array[String] = chaine.split(' ')
            var listeNbCols: Array[Int] = chaineSplit.map(x => x.length)
            var numColMax: Int = listeNbCols.reduceLeft(_ min _)-1
            var numLigMax: Int = chaineSplit.length-1
            if ((numLigMax+1 < 6) || (numColMax+1 < 7)) {
                println("Chaine trop courte : plateau vide par défaut de 6*7 créé.")
                Array.ofDim[Int](7,6)
            } else {
                var plateau: Plateau = Array.ofDim[Int](numColMax+1, numLigMax+1)
                importPlateauRT(plateau, chaineSplit, 0, 0, numColMax, numLigMax)
            }
        }
    }

    // (sous-fonction de récursion de la fonction "exportPlateau()")
    def exportPlateauRT(chaine: String, plateau: Plateau, numColonne: Int, numLigne: Int, numColMax: Int, numLigMax: Int): String = {
        (numColonne, numLigne, plateau(numColonne)(numLigne)) match {
            case (`numColMax`, `numLigMax`, 0) => chaine+"."
            case (`numColMax`, `numLigMax`, 1) => chaine+"J"
            case (`numColMax`, `numLigMax`, _) => chaine+"R"
            case (`numColMax`, _, 0) => exportPlateauRT(chaine+".\n", plateau, 0, numLigne+1, numColMax, numLigMax)
            case (`numColMax`, _, 1) => exportPlateauRT(chaine+"J\n", plateau, 0, numLigne+1, numColMax, numLigMax)
            case (`numColMax`, _, _) => exportPlateauRT(chaine+"R\n", plateau, 0, numLigne+1, numColMax, numLigMax)
            case (_, _, 0) => exportPlateauRT(chaine+". ", plateau, numColonne+1, numLigne, numColMax, numLigMax)
            case (_, _, 1) => exportPlateauRT(chaine+"J ", plateau, numColonne+1, numLigne, numColMax, numLigMax)
            case (_, _, _) => exportPlateauRT(chaine+"R ", plateau, numColonne+1, numLigne, numColMax, numLigMax) 
        }
    }

    // fonction qui renvoie une chaine de caractères contenant toutes les lignes du plateau "plateau"
    // exemple d'utilisation : "exportPlateau(plateau)" renvoie la chaine "RJJJRJR JRJRRJR JRJRJJ. .R.R... ....... ......." si "plateau" est le plateau de la page 2 de l'énoncé du projet
    def exportPlateau(plateau: Plateau): String = {
        exportPlateauRT("", plateau, 0, 0, plateau.length-1, plateau(0).length-1)
    }

    // fonction qui renvoie la chaine des numéros de colonne du plateau "plateau"
    def constrChaineNumColsRT(chaine: String, numColonne: Int, nbColonnes: Int): String = {
        (numColonne+1) match {
            case (`nbColonnes`) => chaine+numColonne.toString
            case (_) => constrChaineNumColsRT(chaine+numColonne.toString+" ", numColonne+1, nbColonnes)
        }
    }

    // procédure qui affiche le plateau "plateau" dans la console
    def afficherPlateau(plateau: Plateau): Unit = {
        println("\n"+constrChaineNumColsRT("", 0, plateau.length))
        println(exportPlateau(plateau)+"\n")
    }

    // (sous-procédure de récursion de la procédure "jouerColonne()")
    def jouerColonneRT(plateau: Plateau, numColonne: Int, numLigne: Int, couleurJoueur: Int, numLigMax: Int): Unit = {
        (numLigne, plateau(numColonne)(numLigne)) match {
            case (`numLigMax`, 0) => plateau(numColonne)(numLigMax) = couleurJoueur
            case (_, 0) => jouerColonneRT(plateau, numColonne, numLigne+1, couleurJoueur, numLigMax)
            case (_, _) => plateau(numColonne)(numLigne-1) = couleurJoueur
        }
    }

    // fonction qui renvoie "true" si la colonne "numColonne" n'est pas remplie, et dans ce cas elle place un jeton de couleur "couleurJoueur" dans la colonne numéro "numColonne" du plateau "plateau". Si la colonne "numColonne" est remplie ou dépasse les limites du plateau, la fonction renvoie "false" et ne touche pas au plateau
    def jouerColonne(plateau: Plateau, numColonne: Int, couleurJoueur: Int): Boolean = {
        (estDispoColonne(plateau, numColonne)) match {
            case (false) => false
            case (_) => {
                jouerColonneRT(plateau, numColonne, 0, couleurJoueur, plateau(0).length-1)
                true
            }
        }
    }

    // procédure destinée au joueur humain qui lui fait jouer 1 jeton de couleur "couleurHumain" dans la colonne "numColonne" du plateau "plateau"
    def joueCoupHumain(plateau: Plateau, numColonne: Int, couleurJoueur: Int) : Unit = {
        (jouerColonne(plateau, numColonne, couleurJoueur)) match {
            case (true) => Unit
            case (_) => {
                print("Coup impossible : colonne "+numColonne.toString()+" pleine ou colonne hors limites. Rentrer une autre colonne : ")
                var numColNouvelle: Int = saisieEntier
                joueCoupHumain(plateau, numColNouvelle, couleurJoueur)       
            }
        }
    }

    //fonction qui vérifie si l'entier saisie est dans l'intervalle debut fin
    def verifsaisieintervalle(debut: Int, fin : Int, saisie :Int) : Int={
        ((saisie>=debut),(saisie<=fin)) match {
            case (true,true) => saisie
            case (_,_) => {
                print("Veuillez entrer un nombre compris entre "+debut.toString()+" et "+fin.toString)
                var newsaisie = saisieEntier
                verifsaisieintervalle(debut,fin,newsaisie)
            }
        }
    }

    //fonction qui attribut un score à une configuration fenetre
    def valeurFenetre(fenetre : Array[Int], couleur: Int) : Int={

        var coulAdv=1+couleur%2 //couleur adversaire 
        var compteIA=0
        
        //évaluation positive
        compteIA=fenetre.count(x=>x==couleur) 
        if (compteIA==4)    
            return 1000
        if (compteIA==3 && fenetre.count(x=>x==0)==1)        
            return 50 
        if (compteIA==2 && fenetre.count(x=>x==0)==2)        
            return 5
        if (compteIA==1 && fenetre.count(x=>x==0)==3)        
            return 1
       	
        //évaluation négative
        var compteAdv=0  
        compteAdv=fenetre.count(x=>x==coulAdv)
        //println("COMPTE ADV -------------------------"+compteAdv.toString())
        if (compteAdv==4)
            return -1000
        if (compteAdv==3 && fenetre.count(x=>x==0)==1){             
            return -50
        }
        if (compteAdv==2 && fenetre.count(x=>x==0)==2)        
            return -5
        if (compteAdv==1 && fenetre.count(x=>x==0)==3){ 
            
            return -1
        }
      
      0
    }

    // fonction qui renvoie "true" si la colonne "numColonne" est dispo et "false" sinon
    def estDispoColonne(plateau: Plateau, numColonne: Int): Boolean = {
        if ((numColonne > plateau.length-1) || (numColonne < 0)) {
            false
        } else {
            (plateau(numColonne)(0)) match {
                case (0) => true
                case (_) => false
            }
        }
    }


    // (sous-fonction de récursion de listerColsDispo())
    def listerColsDispoRT(plateau: Plateau, listeCols: Array[Int], numColonne: Int, numColMax: Int): Array[Int] = {
        (numColonne, estDispoColonne(plateau, numColonne)) match {
            case (`numColMax`, true) => listeCols :+ numColMax
            case (`numColMax`, _) => listeCols
            case (_, true) => listerColsDispoRT(plateau, listeCols :+ numColonne, numColonne+1, numColMax)
            case (_,_) => listerColsDispoRT(plateau, listeCols, numColonne+1, numColMax)
        }
    }

    // fonction qui renvoie le tableau des indices des colonnes disponibles dans le plateau "plateau"
    def listerColsDispo(plateau: Plateau): Array[Int] = {
        listerColsDispoRT(plateau, Array(), 0, plateau.length-1)
    }

    // rappel convention : couleurJoueur = 1 ou 2
    // fonction qui renvoie la valeur du meilleur coup à jouer pour le joueur jouant la couleur “couleurJoueurRef”, à partir du plateau “plateau”
    def minMax(plateau: Plateau, couleurJoueurRef: Int, couleurJoueur: Int, profondeur: Int): Int = {
        var cols: Array[Int] = listerColsDispo(plateau)
        if (plateauPlein(plateau) || aGagne(plateau, couleurJoueurRef) || aGagne(plateau, couleurJoueurRef%2+1) || (profondeur == 0)) {
            (plateauPlein(plateau),aGagne(plateau, couleurJoueurRef),aGagne(plateau, couleurJoueurRef%2+1)) match{
                case (_,true,_) => 10000*(profondeur+1)
                case (_,_,true) => -10000*(profondeur+1)
                case (true,_,_) => 0
                case (_,_,_) => valeurCoup(plateau, couleurJoueurRef)

            }
            
        } else if (couleurJoueur == couleurJoueurRef) {
            var value: Int = -1000000
            var i = cols(0)

            for (i <- cols) {
                var plateau2: Plateau = Array.ofDim[Int](plateau.length, plateau(0).length)
                plateau2 = plateau.map(_.clone)
                jouerColonne(plateau2, i, couleurJoueur)
                value = scala.math.max(value, minMax(plateau2, couleurJoueurRef, couleurJoueur%2+1, profondeur-1))
            }
            value


        } else {
            var value: Int = 1000000
            var i = cols(0)
            for (i <- cols) {
                var plateau2: Plateau = Array.ofDim[Int](plateau.length, plateau(0).length)
                plateau2 = plateau.map(_.clone)
                jouerColonne(plateau2, i, couleurJoueur)
                value = scala.math.min(value, minMax(plateau2, couleurJoueurRef, couleurJoueur%2+1, profondeur-1))
            }
            value
        }
    }
    // fonction qui renvoie l’indice de la meilleure colonne à jouer sur le coup suivant pour le joueur jouant la couleur “couleurJoueur”, sur le plateau “plateau”
    // initialiser couleurJoueur avec couleurJoueurRef
    def iaCoup(plateau: Plateau, couleurJoueur: Int, profondeur: Int): Int = {
        var cols: Array[Int] = listerColsDispo(plateau)
        var value: Int = -1000000
        var result: Int = 0
        var i = cols(0)
        for (i <- cols) {
            var plateau2: Plateau = Array.ofDim[Int](plateau.length, plateau(0).length)
            plateau2 = plateau.map(_.clone)
            jouerColonne(plateau2, i, couleurJoueur)
            var newValue: Int = minMax(plateau2, couleurJoueur, couleurJoueur%2+1, profondeur-1)
            if (newValue > value) {
                value = newValue
                result = i
            }
            if (newValue==value){
                var rand= new Random
                if (rand.nextInt(2)==0){
                    value = newValue
                    result = i
                }
            }
        }
        result
    }

    // (sous-fonction de récursion de inverse(plateau))
    def inverseRT(plateau : Plateau, ligne : Int, colonne : Int, res : Plateau, nbColonnes : Int) : Plateau ={
        if (ligne!=(-1)){
            res(ligne)(colonne)=plateau(colonne)(ligne)
            if (colonne!=0){
                inverseRT(plateau,ligne,colonne-1,res,nbColonnes)
            }else{
                inverseRT(plateau,ligne-1,nbColonnes,res,nbColonnes)
            }
        }else{
            res
        }
    }

    //fonction qui retourne un plateau (inverse les lignes et les colonnes)
    def inverse(plateau : Plateau): Plateau={
        var enlignes=Array.ofDim[Int](plateau(0).length,plateau.length)
        inverseRT(plateau , plateau(0).length-1, plateau.length-1, enlignes, plateau.length-1)
        enlignes
    }

    // (sous-fonction de récursion de valeurCoupHor())
    def valeurCoupHorRT(plateauVirtInv: Plateau, fenetre: Array[Int], valeur: Int, couleur: Int, l: Int, c: Int, numLigMax: Int, numColMax: Int): Int = {
        (l, c) match {
            case (`numLigMax`, `numColMax`) => {
                fenetre(0)=plateauVirtInv(l)(c)
                fenetre(1)=plateauVirtInv(l)(c+1)
                fenetre(2)=plateauVirtInv(l)(c+2)
                fenetre(3)=plateauVirtInv(l)(c+3)                
                valeur+valeurFenetre(fenetre,couleur)                
            }
            case (_, `numColMax`) => {
                fenetre(0)=plateauVirtInv(l)(c)
                fenetre(1)=plateauVirtInv(l)(c+1)
                fenetre(2)=plateauVirtInv(l)(c+2)
                fenetre(3)=plateauVirtInv(l)(c+3)                  
                valeurCoupHorRT(plateauVirtInv, fenetre, valeur+valeurFenetre(fenetre,couleur), couleur, l+1, 0, numLigMax, numColMax)
            }
            case (_, _) => {
                fenetre(0)=plateauVirtInv(l)(c)
                fenetre(1)=plateauVirtInv(l)(c+1)
                fenetre(2)=plateauVirtInv(l)(c+2)
                fenetre(3)=plateauVirtInv(l)(c+3)                  
                valeurCoupHorRT(plateauVirtInv, fenetre, valeur+valeurFenetre(fenetre,couleur), couleur, l, c+1, numLigMax, numColMax)
            }
        }
    }

    //fonction qui attribut un score à un plateau en fonction des pionts alligné horizontalement sur le plateau
    def valeurCoupHor(plateauVirt: Plateau, couleur: Int) : Int = {
        var valeur=0 

        //afficherPlateau(plateauVirt)
        val plateauVirtInv: Plateau = inverse(plateauVirt)
        var fenetre = Array(0,0,0,0)

        valeurCoupHorRT(plateauVirtInv, fenetre, valeur, couleur, 0, 0, plateauVirtInv.length-1, plateauVirtInv(0).length-4)

    }

    // (sous-fonction de récursion de valeurCoupVert)
    def valeurCoupVertRT(plateauVirt: Plateau, fenetre: Array[Int], valeur: Int, couleur: Int, l: Int, c: Int, numLigMax: Int, numColMax: Int): Int = {
        (l, c) match {
            case (`numLigMax`, `numColMax`) => {
                fenetre(0)=plateauVirt(l)(c)
                fenetre(1)=plateauVirt(l)(c+1)
                fenetre(2)=plateauVirt(l)(c+2)
                fenetre(3)=plateauVirt(l)(c+3)                
                valeur+valeurFenetre(fenetre,couleur)                
            }
            case (_, `numColMax`) => {
                fenetre(0)=plateauVirt(l)(c)
                fenetre(1)=plateauVirt(l)(c+1)
                fenetre(2)=plateauVirt(l)(c+2)
                fenetre(3)=plateauVirt(l)(c+3)                   
                valeurCoupVertRT(plateauVirt, fenetre, valeur+valeurFenetre(fenetre,couleur), couleur, l+1, 0, numLigMax, numColMax)
            }
            case (_, _) => {
                fenetre(0)=plateauVirt(l)(c)
                fenetre(1)=plateauVirt(l)(c+1)
                fenetre(2)=plateauVirt(l)(c+2)
                fenetre(3)=plateauVirt(l)(c+3)
                valeurCoupVertRT(plateauVirt, fenetre, valeur+valeurFenetre(fenetre,couleur), couleur, l, c+1, numLigMax, numColMax)
            }
        }
    }    

    //fonction qui attribut un score à un plateau en fonction des pionts alligné verticalement sur le plateau
    def valeurCoupVert(plateauVirt: Plateau, couleur: Int) : Int = {
        var valeur=0      
     
        var fenetre = Array(0,0,0,0)      

        valeurCoupVertRT(plateauVirt, fenetre, valeur, couleur, 0, 0, plateauVirt.length-1, plateauVirt(0).length-4)

    }
    
    // (sous-fonction de récursion de valeurCoupDiag)
    def valeurCoupDiagNordOuestRT(plateauVirt: Plateau, fenetre: Array[Int], valeur: Int, couleur: Int, l: Int, c: Int, numLigMax: Int, numColMax: Int): Int = {
        (l, c) match {
            case (`numLigMax`, `numColMax`) => {
                fenetre(0)=plateauVirt(l)(c)
                fenetre(1)=plateauVirt(l+1)(c+1)
                fenetre(2)=plateauVirt(l+2)(c+2)
                fenetre(3)=plateauVirt(l+3)(c+3)                      
                valeur+valeurFenetre(fenetre,couleur)              
            }
            case (_, `numColMax`) => {
                fenetre(0)=plateauVirt(l)(c)
                fenetre(1)=plateauVirt(l+1)(c+1)
                fenetre(2)=plateauVirt(l+2)(c+2)
                fenetre(3)=plateauVirt(l+3)(c+3)                 
                valeurCoupDiagNordOuestRT(plateauVirt, fenetre, valeur+valeurFenetre(fenetre,couleur), couleur, l+1, 0, numLigMax, numColMax)
            }
            case (_, _) => {
                fenetre(0)=plateauVirt(l)(c)
                fenetre(1)=plateauVirt(l+1)(c+1)
                fenetre(2)=plateauVirt(l+2)(c+2)
                fenetre(3)=plateauVirt(l+3)(c+3)                 
                valeurCoupDiagNordOuestRT(plateauVirt, fenetre, valeur+valeurFenetre(fenetre,couleur), couleur, l, c+1, numLigMax, numColMax)
            }
        }
    }    

    // (sous-fonction de récursion de valeurCoupDiag)
    def valeurCoupDiagNordEstRT(plateauVirt: Plateau, fenetre: Array[Int], valeur: Int, couleur: Int, l: Int, c: Int, numLigMax: Int, numColMax: Int): Int = {
        (l, c) match {
            case (`numLigMax`, `numColMax`) => {
                fenetre(0)=plateauVirt(l)(c)
                fenetre(1)=plateauVirt(l+1)(c-1)
                fenetre(2)=plateauVirt(l+2)(c-2)
                fenetre(3)=plateauVirt(l+3)(c-3)                
                valeur+valeurFenetre(fenetre,couleur)              
            }
            case (_, `numColMax`) => {
                fenetre(0)=plateauVirt(l)(c)
                fenetre(1)=plateauVirt(l+1)(c-1)
                fenetre(2)=plateauVirt(l+2)(c-2)
                fenetre(3)=plateauVirt(l+3)(c-3)                  
                valeurCoupDiagNordEstRT(plateauVirt, fenetre, valeur+valeurFenetre(fenetre,couleur), couleur, l+1, 3, numLigMax, numColMax)
            }
            case (_, _) => {
                fenetre(0)=plateauVirt(l)(c)
                fenetre(1)=plateauVirt(l+1)(c-1)
                fenetre(2)=plateauVirt(l+2)(c-2)
                fenetre(3)=plateauVirt(l+3)(c-3)                  
                valeurCoupDiagNordEstRT(plateauVirt, fenetre, valeur+valeurFenetre(fenetre,couleur), couleur, l, c+1, numLigMax, numColMax)
            }
        }
    }

    //fonction qui attribut un score à un plateau en fonction des pionts alligné en diagonal sur le plateau
    def valeurCoupDiag(plateauVirt: Plateau, couleur: Int) : Int = {
        var valeur=0
        var fenetre=Array(0,0,0,0)
      
        var l=0
        var c=0
        valeur = valeurCoupDiagNordOuestRT(plateauVirt, fenetre, valeur, couleur, 0, 0, plateauVirt.length-4, plateauVirt(0).length-4)
     
        valeurCoupDiagNordEstRT(plateauVirt, fenetre, valeur, couleur, 0, 3, plateauVirt.length-4, plateauVirt(0).length-1)
        
    }

    // fonction qui attribut un score globale à un plateau donné  
    def valeurCoup(plateauVirt: Plateau, couleur:Int): Int={ 
       
      var valeur=0
      valeur+= valeurCoupHor(plateauVirt,couleur)+valeurCoupVert(plateauVirt,couleur)+valeurCoupDiag(plateauVirt,couleur) 
      valeur+= plateauVirt(plateauVirt.length/2).count(x=>x==couleur)
      valeur
    }

// (sous-fonction de récursion de meilleurCoup())
def meilleurCoupRT(plateau:Plateau,couleur:Int,col:Int,meilleurCoup:Int,valMeilleurCoup:Int) : Int ={
      col match{
        case (-1)=> {
          meilleurCoup
        }
        case (_)=>{
              var valeurCoupA=0
            
            var plateauVirt: Plateau = Array.ofDim[Int](plateau.length,plateau(0).length)
            plateauVirt=plateau.map(_.clone)
            var fenetre = Array(0,0,0,0)
            
            if(!jouerColonne(plateauVirt,col,couleur)){

                meilleurCoupRT(plateau,couleur,col-1,meilleurCoup,valMeilleurCoup)
            }
            else{
                valeurCoupA += valeurCoup(plateauVirt,couleur)   
                
                if (col==plateauVirt.length/2) valeurCoupA +=3
                       
                if (valeurCoupA > valMeilleurCoup){
                    meilleurCoupRT(plateau,couleur,col-1,col,valeurCoupA)
                }else if(valeurCoupA == valMeilleurCoup){
                    var rand= new Random
                    if (rand.nextInt(2)==0){
                        meilleurCoupRT(plateau,couleur,col-1,col,valeurCoupA)
                    }else{
                        meilleurCoupRT(plateau,couleur,col-1,meilleurCoup,valMeilleurCoup)
                    }
                }else{
                    meilleurCoupRT(plateau,couleur,col-1,meilleurCoup,valMeilleurCoup)
                }
            }        
        }
      }
    }

    // fonction qui renvoie l'indice de la meilleure colonne à jouer pour le joueur de couleur "couleur" et dans le plateau "plateau"
    def meilleurCoup(plateau: Plateau,couleur: Int) : Int = {
        val r = scala.util.Random
        var meilleurCoup=r.nextInt(plateau.length)
        var valMeilleurCoup= -10000
        //var valeurCoupA=-0
      
        meilleurCoupRT(plateau,couleur,plateau.length,meilleurCoup,valMeilleurCoup)
       
    }

    // (sous-fonction de récursion de "plateauPlein()")
    def plateauPleinRT(plateau: Plateau, numColonne: Int, numColMax: Int): Boolean = {
        (numColonne, (plateau(numColonne)(0) == 1) || (plateau(numColonne)(0) == 2)) match {
            case (`numColMax`, true) => true
            case (`numColMax`, _) => false
            case (_, false) => false
            case (_, _) => plateauPleinRT(plateau, numColonne+1, numColMax)
        }
    }

    // fonction qui renvoie "true" si le plateau est plein et "false" sinon
    def plateauPlein(plateau: Plateau): Boolean = {
        plateauPleinRT(plateau, 0, plateau.length-1)
    }

    // (sous-fonction de récursion de "aGagneVertical()")
    def aGagneVerticalRT(plateau: Plateau, numColonne: Int, numLigne: Int, couleurJoueur: Int, numColMax: Int, numLigMax: Int): Boolean = {
        (numColonne, numLigne, (plateau(numColonne)(numLigne) == couleurJoueur) && (plateau(numColonne)(numLigne-1) == couleurJoueur) && (plateau(numColonne)(numLigne-2) == couleurJoueur) && (plateau(numColonne)(numLigne-3) == couleurJoueur)) match {  
            case (_, _, true) => true
            case (`numColMax`, 3, _) => false
            case (_, 3, _) => aGagneVerticalRT(plateau, numColonne+1, numLigMax, couleurJoueur, numColMax, numLigMax)
            case (_, _, _) => aGagneVerticalRT(plateau, numColonne, numLigne-1, couleurJoueur, numColMax, numLigMax)
        }
    }

    // fonction qui renvoie "true" si 4 jetons de couleur "couleurJoueur" sont alignés verticalement dans une des colonnes du plateau "plateau" et "false" sinon
    def aGagneVertical(plateau: Plateau, couleurJoueur: Int): Boolean = {
        aGagneVerticalRT(plateau, 0, plateau(0).length-1, couleurJoueur, plateau.length-1, plateau(0).length-1)
    }

    // (sous-fonction de récursion de la fonction "aGagneHorizontal()")
    def aGagneHorizontalRT(plateau: Plateau, numColonne: Int, numLigne: Int, couleurJoueur: Int, numColMax: Int, numLigMax: Int): Boolean = {
        (numColonne+3, numLigne, (plateau(numColonne)(numLigne) == couleurJoueur) && (plateau(numColonne+1)(numLigne) == couleurJoueur) && (plateau(numColonne+2)(numLigne) == couleurJoueur) && (plateau(numColonne+3)(numLigne) == couleurJoueur)) match {
            case (_, _, true) => true
            case (`numColMax`, 0, _) => false
            case (`numColMax`, _, _) => aGagneHorizontalRT(plateau, 0, numLigne-1, couleurJoueur, numColMax, numLigMax)
            case (_, _, _) => aGagneHorizontalRT(plateau, numColonne+1, numLigne, couleurJoueur, numColMax, numLigMax)
        }
    }

    // fonction qui renvoie "true" si 4 jetons de couleur "couleurJoueur" sont alignés horizontalement dans une des lignes du plateau "plateau" et "false" sinon
    def aGagneHorizontal(plateau: Plateau, couleurJoueur: Int): Boolean = {
        aGagneHorizontalRT(plateau, 0, plateau(0).length-1, couleurJoueur, plateau.length-1, plateau(0).length-1)
    }

    // (sous-fonction de récursion de la fonction "aGagneDiagAscVert()")
    def aGagneDiagAscVertRT(plateau: Plateau, numColonne: Int, numLigne: Int, couleurJoueur: Int, numColMax: Int, numLigMax: Int): Boolean = {
        (numColonne+3, numLigne, (plateau(numColonne)(numLigne) == couleurJoueur) && (plateau(numColonne+1)(numLigne-1) == couleurJoueur) && (plateau(numColonne+2)(numLigne-2) == couleurJoueur) && (plateau(numColonne+3)(numLigne-3) == couleurJoueur)) match {  
            case (_, _, true) => true
            case (`numColMax`, 3, _) => false
            case (`numColMax`, _, _) => aGagneDiagAscVertRT(plateau, 0, numLigne-1, couleurJoueur, numColMax, numLigMax)
            case (_, _, _) => aGagneDiagAscVertRT(plateau, numColonne+1, numLigne, couleurJoueur, numColMax, numLigMax)
        }
    }

    // fonction qui renvoie "true" si 4 jetons de couleur "couleurJoueur" sont alignés sur une diagonale ascendante verticale du plateau "plateau" et "false" sinon
    def aGagneDiagAscVert(plateau: Plateau, couleurJoueur: Int): Boolean = {
        aGagneDiagAscVertRT(plateau, 0, plateau(0).length-1, couleurJoueur, plateau.length-1, plateau(0).length-1)
    }

    // (sous-fonction de récursion de la fonction "aGagneDiagDescVert()")
    def aGagneDiagDescVertRT(plateau: Plateau, numColonne: Int, numLigne: Int, couleurJoueur: Int, numColMax: Int, numLigMax: Int): Boolean = {
        (numColonne+3, numLigne+3, (plateau(numColonne)(numLigne) == couleurJoueur) && (plateau(numColonne+1)(numLigne+1) == couleurJoueur) && (plateau(numColonne+2)(numLigne+2) == couleurJoueur) && (plateau(numColonne+3)(numLigne+3) == couleurJoueur)) match {  
            case (_, _, true) => true
            case (`numColMax`, `numLigMax`, _) => false
            case (`numColMax`, _, _) => aGagneDiagDescVertRT(plateau, 0, numLigne+1, couleurJoueur, numColMax, numLigMax)
            case (_, _, _) => aGagneDiagDescVertRT(plateau, numColonne+1, numLigne, couleurJoueur, numColMax, numLigMax)
        }
    }

    // fonction qui renvoie "true" si 4 jetons de couleur "couleurJoueur" sont alignés sur une diagonale descendante verticale du plateau "plateau" et "false" sinon
    def aGagneDiagDescVert(plateau: Plateau, couleurJoueur: Int): Boolean = {
        aGagneDiagDescVertRT(plateau, 0, 0, couleurJoueur, plateau.length-1, plateau(0).length-1)
    }

    // fonction qui renvoie "true" si le joueur de couleur "couleurJoueur" a gagné sur le plateau "plateau"
    def aGagne(plateau: Plateau, couleurJoueur: Int) : Boolean = {
        aGagneHorizontal(plateau, couleurJoueur) || aGagneVertical(plateau, couleurJoueur) || aGagneDiagAscVert(plateau, couleurJoueur) || aGagneDiagDescVert(plateau, couleurJoueur)
    }
    //
    //
    //Rappel : la fonction jouer remplace la fonction joueCoupOrdi car nous avons plusieurs IA
    //
    //
    //procédure qui joue un coup dans le plateau en fonction du type de joueur
    def jouer(plateau : Plateau, couleurJoueur : Int, typ : Int, diff : Int): Unit={
        (typ) match {
            case (1) =>{
                print("Quelle colonne voulez-vous jouer ? (0-6) \n")
                var numColonne: Int = saisieEntier  // demande de la colonne au joueur
                joueCoupHumain(plateau, numColonne,couleurJoueur)  // coup du joueur
                print("Voulez vous exporter la partie dans l'état actuel \n")
                println("Taper : ")
                println("1 : Oui")
                println("2 : Non, continuer la partie")
                var nb: Int = verifsaisieintervalle(1,2,saisieEntier)
                if (nb==1){
                    var sortie= exportPlateau(plateau)
                    println(sortie)
                    sys.exit(0)
                }
            }
            case (2) =>{
                var r = new scala.util.Random
                var numColonne=r.nextInt(plateau.length)
                println("Je suis l'ordi et je joue : "+numColonne.toString())
                jouerColonne(plateau, numColonne, couleurJoueur)
            }
            case (3) => jouerColonne(plateau,meilleurCoup(plateau,couleurJoueur),couleurJoueur)
            case (_) => jouerColonne(plateau, iaCoup(plateau, couleurJoueur, diff), couleurJoueur) // profondeur_pourrie
        }
    }

    //(sous-fonction de récursion de la fonction partie)
    def partieRT (plateau : Plateau, aGagneBool : Boolean, plateauPleinBool : Boolean, couleurJoueur : Int, j1Type : Int, j2Type : Int, j1Diff : Int, j2Diff : Int, tour : Int): Unit={
        (aGagneBool,plateauPleinBool) match{
            case (false,false) =>{
                println("================ Tour "+(tour+1).toString())
                if (couleurJoueur==1){ //c'est le tour du joueur Jaune
                    println("Jaune :")
                    jouer(plateau,couleurJoueur,j1Type,j1Diff)
                }else{//c'est le tour de joueur Rouge
                    println("Rouge :")
                    jouer(plateau,couleurJoueur,j2Type,j2Diff)
                }
                afficherPlateau(plateau)
                partieRT(plateau,aGagne(plateau,couleurJoueur),plateauPlein(plateau),couleurJoueur%2+1,j1Type,j2Type,j1Diff,j2Diff,tour+1)
            }
            case (true,_) =>{
                if ((couleurJoueur%2+1)==1){
                    println("Le joueur jaune a gagné")
                }else{
                    println("Le joueur rouge a gagné")
                }
            }
            case (_,_) =>{
                println("Match null")
            }

        }
    }

    //fonction qui implémente une parti de puissance4 
    def partie(plateau : Plateau) : Unit ={
        var couleurJoueur: Int =0 
        var aGagneB: Boolean = false  // booléen qui dit si le joueur courant a gagné
        var plateauPleinB: Boolean = false  // booléen qui dit si le plateau est plein
        val r = new scala.util.Random
        couleurJoueur = 1+r.nextInt(2)//on choisit au hasard qui commence
        var tour=0
        println("================ Veuillez choisir de quel type est le joueur Jaune") 
        print("Taper : \n")
        print("1 : Si le joueur jaune est un humain \n")
        print("2 : Si le joueur jaune est une IA de niveau 1 (random)\n")
        print("3 : Si le joueur jaune est une IA de niveau 2 (meilleur coup)\n")
        print("4 : Si le joueur jaune est une IA de niveau 3 (min max)\n")

        var jauneType: Int = verifsaisieintervalle(1,4,saisieEntier)
        var jaunePronf : Int = 0
        if (jauneType==4){
            println("================ Veuillez entrer un entier supérieur ou égale à 1 pour règler la difficulté de l'IA")
            jaunePronf = verifsaisieintervalle(1,10,saisieEntier)
        }
        println("================ Veuillez choisir de quel type est le joueur Rouge") 
        print("Taper : \n")
        print("1 : Si le joueur rouge est un humain \n")
        print("2 : Si le joueur rouge est une IA de niveau 1 (random)\n")
        print("3 : Si le joueur rouge est une IA de niveau 2 (meilleur coups)\n")
        print("4 : Si le joueur rouge est une IA de niveau 3 (min max)\n")

        var rougeType: Int = verifsaisieintervalle(1,4,saisieEntier)
        var rougePronf : Int = 0
        if (rougeType==4){
            println("================ Veuillez entrer un entier supérieur ou égal à 1 pour régler la difficulté de l'IA")
            rougePronf = verifsaisieintervalle(1,10,saisieEntier)
        }
        partieRT(plateau,aGagneB,plateauPleinB,couleurJoueur,jauneType,rougeType,jaunePronf,rougePronf,tour)
        
    }
    
    def main(args: Array[String]): Unit = {
        var couleurJoueur: Int =0 
                                                  
        println("\n================ Bienvenue dans le générateur de puissance 4") 
        print("Taper : \n")
        print("1 : Si vous voulez jouer une nouvelle partie \n")
        print("2 : Si vous voulez importer une partie \n")

        var debutpart: Int = verifsaisieintervalle(1,2,saisieEntier)
        var plateau: Plateau = Array.ofDim[Int](1, 1)   // initialisation du plateau aux valeur par défaut
        if (debutpart==1){
            print("Saisir le nombre de colonnes : ")
            var nbColonnes: Int = saisieNbColonnes
            print("Saisir le nombre de lignes : ")
            var nbLignes: Int = saisieNbLignes
            plateau = Array.ofDim[Int](nbColonnes, nbLignes)
        }else{
            print("Saisir le plateau à importer : ")
            var chainePlateau: String = scala.io.StdIn.readLine()
            plateau = importPlateau(chainePlateau) 
        }        
        
        afficherPlateau(plateau)
        partie(plateau)
    }
}
