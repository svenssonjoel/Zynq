

#include <clc.h>

typedef unsigned int uint32_t;



/* One work-group per color channel
 *
 * For images of size 1024*1024*X (X color channels of type float)
 *
 */

/* number of buckets (resolution of histogram) */
#define BUCKETS 256

#define IMAGE_WIDTH  1024
#define IMAGE_HEIGHT 1024

uint32_t bucket(float color) {
	if (color > 1.0f) color = 1.0f; /* clamp */
	if (color < 0.0f) color = 0.0f;

	return floor(color * (float)(BUCKETS-1));
}


__kernel void __attribute__((reqd_work_group_size(256,1,1)))
	histo(__global float *color_channel, __global uint32_t* out){

	/* Produce entire histogram (per color channel) into shared local storage */
	__local uint32_t histogram[128][BUCKETS];
	__local float    image_data[2048];
	int active = 1024;
	int next   = 0;
	int tmp;

	event_t          trans_complete;

	unsigned int x = get_local_id(0);
	async_work_group_copy(image_data + next,color_channel, 1024,trans_complete);


	for (unsigned int y = 0; y < 1023; y ++) {
		/* wait for first transfer to finish */
		wait_group_events(1,&trans_complete);

		/* Swap active and next */
		tmp = active;
		active = next;
		next = tmp;

		/* initiate transfer of data to be used in next iteration */
		async_work_group_copy(image_data + next,color_channel + ((y + 1) * 1024), 1024,trans_complete);

		//barrier(CLK_LOCAL_MEM_FENCE);

		/* These has to happen sequentially within a single Work item.
		 * Parallelism between work items should be allowed.
		 */
		if (x < 128){ /* use only 128 work items here */
			histogram[x][bucket(image_data[active + (x * 8)])]++;
			histogram[x][bucket(image_data[active + (x * 8) + 1])]++;
			histogram[x][bucket(image_data[active + (x * 8) + 2])]++;
			histogram[x][bucket(image_data[active + (x * 8) + 3])]++;
			histogram[x][bucket(image_data[active + (x * 8) + 4])]++;
			histogram[x][bucket(image_data[active + (x * 8) + 5])]++;
			histogram[x][bucket(image_data[active + (x * 8) + 6])]++;
			histogram[x][bucket(image_data[active + (x * 8) + 7])]++;
		}


	}

	wait_group_events(1,&trans_complete);
	/* the last step */
	if (x < 128){ /* use only 128 work items here */
		histogram[x][bucket(image_data[next + (x * 8)])]++;
		histogram[x][bucket(image_data[next + (x * 8) + 1])]++;
		histogram[x][bucket(image_data[next + (x * 8) + 2])]++;
		histogram[x][bucket(image_data[next + (x * 8) + 3])]++;
		histogram[x][bucket(image_data[next + (x * 8) + 4])]++;
		histogram[x][bucket(image_data[next + (x * 8) + 5])]++;
		histogram[x][bucket(image_data[next + (x * 8) + 6])]++;
		histogram[x][bucket(image_data[next + (x * 8) + 7])]++;
	}

	barrier(CLK_LOCAL_MEM_FENCE);

	/* reduce histograms */

	/* Here using 256 work items */

	uint32_t sum = 0;
	for (unsigned int i = 0; i < 128; i ++) {
		sum += histogram[i][x];
	}
	out[x] = sum;




	//async_work_group_copy(out,histogram[0],BUCKETS*sizeof(uint32_t),0);

}
