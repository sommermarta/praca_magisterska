#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "svm.h"

char* line;
int max_line_len = 1024;
struct svm_node *x;
int max_nr_attr = 64;

struct svm_model* model;
int predict_probability=0;
int dense = 0;

void predict(FILE *input, FILE *output)
{
	int correct = 0;
	int total = 0;
	double error = 0;
	double sumv = 0, sumy = 0, sumvv = 0, sumyy = 0, sumvy = 0;

	int svm_type=svm_get_svm_type(model);
	int nr_class=svm_get_nr_class(model);
	int *labels=(int *) malloc(nr_class*sizeof(int));
	double *prob_estimates=NULL;
	int j;

	if(predict_probability)
	{
		if (svm_type==NU_SVR || svm_type==EPSILON_SVR)
			printf("Prob. model for test data: target value = predicted value + z,\nz: Laplace distribution e^(-|z|/sigma)/(2sigma),sigma=%g\n",svm_get_svr_probability(model));
		else
		{
			svm_get_labels(model,labels);
			prob_estimates = (double *) malloc(nr_class*sizeof(double));
			fprintf(output,"labels");		
			for(j=0;j<nr_class;j++)
				fprintf(output," %d",labels[j]);
			fprintf(output,"\n");
		}
	}
	while(1)
	{
		double target,v;

		if(dense){
			int i = 0;
			int index = 1;
			int c;

			while(1)
			{
				double tmp;

				if (fscanf(input, "%lf", &tmp) == EOF)
					goto show;
	
				c = fgetc(input);
				//read in consecutive spaces until the last one or newline
				while (isspace(c) && c != '\n'){
					int cnext = fgetc(input);
					if (!isspace(cnext)){
						ungetc(cnext, input);
						break;
					}
					c = cnext;				
				}

				//newline indicates label
				if (c == '\n' || c == EOF){
					target = tmp;
					x[i++].index = -1;
					break;
				}
				else if (isspace(c)){
					if (tmp != 0){
						if(i>=max_nr_attr-1)	// need one more for index = -1
						{
							max_nr_attr *= 2;
							x = (struct svm_node *) realloc(x,max_nr_attr*sizeof(struct svm_node));
						}
						x[i].index = index;
						x[i].value = tmp;
						++i;
					}
					index++;
				}										
				else{
					fprintf(stderr, "unknown character in format: %c\n", c);
					exit(1);
				}
			}
		}
		else{
			int i = 0;
			int c;
	
			if (fscanf(input,"%lf",&target)==EOF)
				break;
	
			while(1)
			{
				if(i>=max_nr_attr-1)	// need one more for index = -1
				{
					max_nr_attr *= 2;
					x = (struct svm_node *) realloc(x,max_nr_attr*sizeof(struct svm_node));
				}

				do {
					c = getc(input);
					if(c=='\n' || c==EOF) goto out2;
				} while(isspace(c));
				ungetc(c,input);
				fscanf(input,"%d:%lf",&x[i].index,&x[i].value);
				++i;
			}	

	out2:
			x[i++].index = -1;
		}

		if (predict_probability && (svm_type==C_SVC || svm_type==C_SVC))
		{
			v = svm_predict_probability(model,x,prob_estimates);
			fprintf(output,"%g ",v);
			for(j=0;j<nr_class;j++)
				fprintf(output,"%g ",prob_estimates[j]);
			fprintf(output,"\n");
		}
		else
		{
			v = svm_predict(model,x);
			fprintf(output,"%g\n",v);
		}

		if(v == target)
			++correct;
		error += fabs(v-target);
		sumv += v;
		sumy += target;
		sumvv += v*v;
		sumyy += target*target;
		sumvy += v*target;
		++total;
	}
show:
	printf("Accuracy = %g%% (%d/%d) (classification)\n",
		(double)correct/total*100,correct,total);
	printf("Mean absolute error = %g (regression)\n",error/total);
	printf("Squared correlation coefficient = %g (regression)\n",
		((total*sumvy-sumv*sumy)*(total*sumvy-sumv*sumy))/
		((total*sumvv-sumv*sumv)*(total*sumyy-sumy*sumy))
		);
	if(predict_probability)
	{
		free(prob_estimates);
		free(labels);
	}
}

void exit_with_help()
{
	printf(
	"Usage: svm-predict [options] test_file model_file output_file\n"
	"options:\n"
	"-b probability_estimates: whether to predict probability estimates, 0 or 1 (default 0); one-class SVM not supported yet\n"
	"-D dense: whether to use dense formate in files, 0 or 1 (default 0)\n"
	);
	exit(1);
}

int main(int argc, char **argv)
{
	FILE *input, *output;
	int i;

	// parse options
	for(i=1;i<argc;i++)
	{
		if(argv[i][0] != '-') break;
		++i;
		switch(argv[i-1][1])
		{
			case 'b':
				predict_probability = atoi(argv[i]);
				break;
			case 'D':
				dense = atoi(argv[i]);
				break;
			default:
				fprintf(stderr,"unknown option\n");
				exit_with_help();
		}
	}
	if(i>=argc)
		exit_with_help();
	
	input = fopen(argv[i],"r");
	if(input == NULL)
	{
		fprintf(stderr,"can't open input file %s\n",argv[i]);
		exit(1);
	}

	output = fopen(argv[i+2],"w");
	if(output == NULL)
	{
		fprintf(stderr,"can't open output file %s\n",argv[i+2]);
		exit(1);
	}

	if((model=svm_load_model(argv[i+1]))==0)
	{
		fprintf(stderr,"can't open model file %s\n",argv[i+1]);
		exit(1);
	}
	
	line = (char *) malloc(max_line_len*sizeof(char));
	x = (struct svm_node *) malloc(max_nr_attr*sizeof(struct svm_node));
	if(predict_probability)
		if(svm_check_probability_model(model)==0)
		{
			fprintf(stderr,"Model does not support probabiliy estimates\n");
			exit(1);
		}
	predict(input,output);
	svm_destroy_model(model);
	free(line);
	free(x);
	fclose(input);
	fclose(output);
	return 0;
}
