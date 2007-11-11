/*-----------------------------------------------------------------------

File  : fofshared.c

Author: Josef Urban

Contents
 
  Read an initial set of fof terms and print the (shared) codes of all subterms
  present in them. If file names given (or file and stdin), read both
  in, but only print the codes for the second one (this is intended to
  allow consistent codes over several runs).
 

  Copyright 1998, 1999 by the author.
  This code is released under the GNU General Public Licence.
  See the file COPYING in the main CLIB directory for details.
  Run "eprover -h" for contact information.

Changes

<1> Fri Nov 28 00:27:40 MET 1997

-----------------------------------------------------------------------*/

#include <stdio.h>
#include <cio_commandline.h>
#include <cio_output.h>
#include <cte_termbanks.h>
#include <ccl_formulafunc.h>
#include <ccl_proofstate.h>

/*---------------------------------------------------------------------*/
/*                  Data types                                         */
/*---------------------------------------------------------------------*/

typedef enum
{
   OPT_NOOPT=0,
   OPT_HELP,
   OPT_VERBOSE,
   OPT_OUTPUT
}OptionCodes;



/*---------------------------------------------------------------------*/
/*                        Global Variables                             */
/*---------------------------------------------------------------------*/

OptCell opts[] =
{
   {OPT_HELP, 
    'h', "help", 
    NoArg, NULL,
    "Print a short description of program usage and options."},
   {OPT_VERBOSE, 
    'v', "verbose", 
    OptArg, "1",
    "Verbose comments on the progress of the program."},
   {OPT_OUTPUT,
    'o', "output-file",
    ReqArg, NULL,
   "Redirect output into the named file."},
   {OPT_NOOPT,
    '\0', NULL,
    NoArg, NULL,
    NULL}
};

char *outname = NULL;

/*---------------------------------------------------------------------*/
/*                      Forward Declarations                           */
/*---------------------------------------------------------------------*/

CLState_p process_options(int argc, char* argv[]);
void print_help(FILE* out);

/*---------------------------------------------------------------------*/
/*                         Internal Functions                          */
/*---------------------------------------------------------------------*/

int debug = 0;

/* print term codes, setting the TPSpecialFlag, to prevent printing
   more than once; ignore special fof terms */
bool TBPrintTermCompact1(FILE* out, TB_p bank, Term_p term, bool comma)
{
   int i;
   bool res = comma;
   if((!SigQueryFuncProp(bank->sig, term->f_code, FPFOFOp)) && 
      (bank->sig->qex_code !=  term->f_code) &&
      (bank->sig->qall_code !=  term->f_code) &&
      (SIG_TRUE_CODE != term->f_code))
   {
      if(comma) { fputc(',', out); } 
      fprintf(out, "%ld", term->entry_no);
      res = true;
   }
   TermCellSetProp(term, TPSpecialFlag);
   if(!TermIsConst(term))
   {
      assert(term->args && (term->arity>0));
      for(i=0;i<term->arity;i++)
      {
	 if(!TermIsVar(term->args[i]) && (!TermCellQueryProp(term->args[i], TPSpecialFlag)))
	 {
	    res = res | TBPrintTermCompact1(out, bank, term->args[i], res);
	 }
      }
   }
   return res;
}


/* Delete the TPSpecialFlag recursively */
void TBRecUnMarkTerm(Term_p term)
{
   PStack_p stack = PStackAlloc();   
   int i;

   assert(term);
   
   PStackPushP(stack, term);
   while(!PStackEmpty(stack))
   {
      term = PStackPopP(stack);
      if(!TermIsVar(term))
      {
	 TermCellDelProp(term, TPSpecialFlag);
	 for(i=0; i<term->arity; i++)
	 {
	    PStackPushP(stack, term->args[i]);
	 }
      }
   }
   PStackFree(stack);
}


int main(int argc, char* argv[])
{
   CLState_p   state;
   Scanner_p   in; 
   Sig_p       sig;
   long        count, size, depth, size_sum, depth_sum, size_max,
               depth_max;
   int         i;
   Term_p      term;
   TB_p        bank;
   bool        sym,com;
   ProofState_p     proofstate;

   assert(argv[0]);
   InitIO(argv[0]);

   state = process_options(argc, argv);
   
   FormulaTermEncoding = true;

   if(state->argc ==  0)
   {
      CLStateInsertArg(state, "-");
   }
   GlobalOut = OutOpen(outname);

   sig = SigAlloc();
   bank = TBAlloc(TPIgnoreProps, sig);
   size_sum = depth_sum = size_max = depth_max = count = 0;


   proofstate = ProofStateAlloc(FPIgnoreProps, NULL, NULL); 
   
   for(i=0; state->argv[i]; i++)
   {
      in = CreateScanner(StreamTypeFile, state->argv[i], true, NULL);
      ScannerSetFormat(in, TSTPFormat);
      
      FormulaAndClauseSetParse(in, proofstate->axioms, 
                               proofstate->f_axioms,
                               proofstate->original_terms, 
                               NULL);
      WFormula_p handle;

      handle = proofstate->f_axioms->anchor->succ;
   
      while(handle!=proofstate->f_axioms->anchor)
      {

	 if(debug == 1)
	 {
	    TBPrintTermCompact(GlobalOut, proofstate->original_terms, handle->tformula); 
	    fprintf(GlobalOut, "\n");
	 }
	 fprintf(GlobalOut, "terms(%s,[", handle->info->name);
	 TBPrintTermCompact1(GlobalOut, proofstate->original_terms, handle->tformula, false);
	 TBRecUnMarkTerm(handle->tformula);
	 fprintf(GlobalOut, "]).\n");
	 handle = handle->succ;
      }

       CheckInpTok(in, NoToken);
      DestroyScanner(in);
   }


/*    for(i=0; state->argv[i]; i++) */
/*    {    */
/*       in = CreateScanner(StreamTypeFile, state->argv[i], true, NULL); */
/*       ScannerSetFormat(in, TSTPFormat); */
/*       while(!TestInpTok(in,NoToken)) */
/*       { */
/* 	 /\* term  = TBTermParse(in, bank); *\/ */
/* 	 term = TFormulaTPTPParse(in, bank); */
/* 	 size  = TermWeight(term,1,1); */
/* 	 depth = TermDepth(term); */
/* 	 if(term->arity == 2) */
/* 	 { */
/* 	    sym = (term->args[0]==term->args[1]); */
/* 	 } */
/* 	 else */
/* 	 { */
/* 	    sym = false; */
/* 	 } */
/* 	 if((term->arity == 2) && (term->args[0]->arity == 1)) */
/* 	 { */
/* 	    com = (term->args[0]->args[1]==term->args[1]); */
/* 	 } */
/* 	 else */
/* 	 { */
/* 	    com = false; */
/* 	 } */
/*	 TBPrintTermCompact(GlobalOut, bank, term); */
/* 	 TermPrint(GlobalOut, term, sig, DEREF_NEVER); */
/* 	 fprintf(GlobalOut, "  : %ld : %ld : %c : %c\n",  */
/* 		 size,depth, sym?'s':'n',com?'s':'n'); */
	 /* TBDelete(bank,term); */
/* 	 count++; */
/* 	 size_sum+=size; */
/* 	 depth_sum+=depth; */
/* 	 size_max = MAX(size_max, size); */
/* 	 depth_max = MAX(depth_max, depth); */
/*       } */
/*       DestroyScanner(in);	  */
/*    } */

/*    fprintf(GlobalOut,  */
/* 	   "# Terms: %ld  ASize: %f MSize: %ld, ADepth: %f MDepth: %ld\n", */
/* 	   count, size_sum/(float)count, size_max, */
/* 	   depth_sum/(float)count, depth_max); */
   bank->sig = NULL;
   TBFree(bank);
   SigFree(sig);
   
   CLStateFree(state);

   ExitIO();
#ifdef CLB_MEMORY_DEBUG
   MemFlushFreeList();
   MemDebugPrintStats(stdout);
#endif

   return 0;
}


/*-----------------------------------------------------------------------
//
// Function: process_options()
//
//   Read and process the command line option, return (the pointer to)
//   a CLState object containing the remaining arguments.
//
// Global Variables: opts, Verbose, TermPrologArgs,
//                   TBPrintInternalInfo 
//
// Side Effects    : Sets variables, may terminate with program
//                   description if option -h or --help was present
//
/----------------------------------------------------------------------*/

CLState_p process_options(int argc, char* argv[])
{
   Opt_p handle;
   CLState_p state;
   char*  arg;
   
   state = CLStateAlloc(argc,argv);
   
   while((handle = CLStateGetOpt(state, &arg, opts)))
   {
      switch(handle->option_code)
      {
      case OPT_VERBOSE:
	    Verbose = CLStateGetIntArg(handle, arg);
	    break;
      case OPT_HELP: 
	    print_help(stdout);
	    exit(NO_ERROR);
      case OPT_OUTPUT:
	    outname = arg;
	    break;
      default:
	 assert(false);
	 break;
      }
   }
   return state;
}
 
void print_help(FILE* out)
{
   fprintf(out, "\n\
\n\
cl_test\n\
\n\
Usage: termprops [options] [files]\n\
\n\
Read a set of terms and print it with size and depth information.\n\
\n");
   PrintOptions(stdout, opts);
}


/*---------------------------------------------------------------------*/
/*                        End of File                                  */
/*---------------------------------------------------------------------*/


