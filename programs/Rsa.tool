/* This program encrypt and decrypt a word in RSA, given the private key, public
   key and the clear text as integers.
*/
/* Author : Mathieu Demarne, 2013 */

object RsaTest {
    def main() : Unit = {
        if (new RsaBox().start(33, 7, 3)) {
            println("End of Execution");
        }
    }
}
 
/* Box representing an RSA cryptographic system. */
class RsaBox {
    var m : Int;
    var e : Int;
    var d : Int;

    /* Initialize and start the computation. Return true if the text is the
    same once decrypted again, false o/w. */
    def start(m_: Int, e_ : Int, d_ : Int) : Bool = {
    
        /* Local variables */
        var cypher : Int;
        var cleartext : Int;
        var i : Int;
        
        /* Get the array list to test */
        var l : Int[];
        l = new Int[6];
        l[0] = 6;
        l[1] = 3;
        l[2] = 27;
        l[3] = 31;
        l[4] = 13;
        l[5] = 10;
        
        /* Setting of the class variables */
        m = m_;
        e = e_;
        d = d_;
    
        
        i = 0;
        while(i < l.length) {
            cypher = this.encryptIterative(l[i]);
            cleartext = this.decryptIterative(cypher);
            println("Iterative : Given text : " + l[i] + ", Cypher : " + cypher + 
            ", Cleartext : " + cleartext);
            println(l[i] == cleartext);
            i = i + 1;
        }
        
        /* Test recursive method */
        i = 0;
        while(i < l.length) {
            cypher = this.encryptRecursive(l[i]);
            cleartext = this.decryptRecursive(cypher);
            println("Recursive : Given text : " + l[i] + ", Cypher : " + cypher + 
            ", Cleartext : " + cleartext);
            println(l[i] == cleartext);
            i = i + 1;
        }
        
        /* Test Bruteforce method */
        
        /* NB : NOT USED, as it generates a StackOverflow in JVM with the 
        interpretor.*/
        /*i = 0;
        while(i < l.length) {
            cypher = this.encryptBruteforce(l[i]);
            cleartext = this.decryptBruteforce(cypher);
            println("Bruteforce : Given text : " + l[i] + ", Cypher : " + cypher + 
            ", cleartext : " + cleartext);
            println(l[i] == cleartext);
            i = i + 1;
        }*/
        
        
        return true;
    }
    
/* ------------------------------ Iterative ----------------------------------*/      

    /* Encrypt the text in parameter (iterative method) */
    def encryptIterative(text : Int) : Int = {
        return this.modpowIterative(text, e);
    }
    
    /* decrypt the cypher in parameter (iterative method) */
    def decryptIterative(cypher : Int) : Int = {
        return this.modpowIterative(cypher, d);
    }
    
    /* Iterative modpow to (en/de)crypt. */
    def modpowIterative(t : Int, key : Int) : Int = {
        var ot : Int;
        ot = t;
        while (1 < key) { 
            t = t*ot;
            t = this.getRest(t);
            key = key -1;
        }
        return this.getRest(t);
    }
    
/* ------------------------------ Recurvise ----------------------------------*/      

    /* Encrypt the text in parameter (recursive method) */
    def encryptRecursive(text : Int) : Int = {
        return this.modpowRecursive(text, e, text);
    }
    
    /* decrypt the cypher in parameter (recursive method) */
    def decryptRecursive(cypher : Int) : Int = {
        return this.modpowRecursive(cypher, d, cypher);
    }
    /* Recursive modpow to (en/de)crypt. */
    def modpowRecursive(t : Int, key : Int, ot : Int) : Int = {
        if (1 < key) { 
            t = t*ot;
            t = this.getRest(t);
            key = key -1;
            t = this.modpowRecursive(t, key, ot);
        }
        return this.getRest(t);
    }
    
/* ------------------------------ Bruteforce ---------------------------------*/     

    /* Encrypt the text in parameter (bruteforce method) */
    def encryptBruteforce(text : Int) : Int = {
        return this.modpowBruteforce(text, e);
    }
    
    /* decrypt the cypher in parameter (bruteforce method) */
    def decryptBruteforce(cypher : Int) : Int = {
        return this.modpowBruteforce(cypher, d);
    }
   
   /* Bruteforce modpow to (en/de)crypt. */
   def modpowBruteforce(t : Int, key : Int) : Int = {
        var ot : Int;
        ot = t;
        while (1 < key) { 
            t = t*ot;
            key = key -1;
        }
        return this.getRest(t);
   }
   
/* ------------------------------ Helpers -----------------------------------*/

    /* Get the residus of the modulo calculus */
    def getRest(t : Int) : Int = {
        while (m < t) {
            t = t - m;
        }
        return t;
    }
}
